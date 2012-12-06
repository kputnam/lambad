{-# LANGUAGE OverloadedStrings #-}

module Language.Lambad.Eval
  ( evalLexical
  , evalDynamic
  , emptyEnv
  , defaultEnv
  , runEval
  ) where

import qualified Data.Map  as M
import qualified Data.Text as T

import Control.Arrow
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Error

import Language.Lambad.Misc
import Language.Lambad.Syntax
import Language.Lambad.Parser
import Language.Lambad.Pretty
import Data.Attoparsec.Text (parseOnly)

type Id
  = T.Text

--------------------------------------------------------------------------------

data Value
  = VClosure Environment Expression

instance Show Value where
  show (VClosure _ e) = "VClosure (" ++ show e ++ ")"

instance Pretty Value where
  pretty (VClosure _ e) = pretty $ Abstraction "_" e

instance Error T.Text where
  noMsg   = ""
  strMsg  = T.pack

--------------------------------------------------------------------------------

type Environment
  = M.Map Id Value

emptyEnv :: Environment
emptyEnv = M.empty

extendEnv :: Id -> Value -> Environment -> Environment
extendEnv = M.insert

-- We're using majiks here, because we want each top-level definition to
-- be able to reference any other top-level definition (including itself)
-- for the sake of convenience. This saves from having to sort definitions
defaultEnv :: Environment
defaultEnv
  = M.fromList $ map (second closure) definitions
  where
    parse s     = T.pack `lmap` parseOnly parseExpr s
    eval s      = runEval defaultEnv =<< evalLexical `fmap` parse s
    closure s   = either (error . T.unpack) id $ eval s
    definitions = [("id"    , "lambda x x")
                  ,("fix"   , "(lambda f lambda x f (x x)) (lambda f lambda x f (x x))")
                  ,("apply" , "lambda f lambda x f x")              -- id
                  ,("const" , "lambda x lambda _ x")
                  ,("flip"  , "lambda f lambda a lambda b f b a")
                  
                  -- Bool : a -> a -> a
                  ,("if"    , "lambda x lambda t lambda f (x t) f")
                  ,("not"   , "lambda x lambda t lambda f (x f) t")
                  ,("true"  , "lambda t lambda f t")
                  ,("false" , "lambda t lambda f f")
                  ,("or"    , "lambda x lambda y (x x) y")
                  ,("and"   , "lambda x lambda y (x y) x")
                  ,("xor"   , "todo")

                  -- Nat : (a -> a) -> a -> a
                  ,("succ"  , "lambda n lambda f lambda x f (n f x)")
                  ,("0"     , "lambda f lambda x x")
                  ,("1"     , "succ 0")
                  ,("2"     , "succ 1")
                  ,("3"     , "succ 2")
                  ,("4"     , "succ 3")
                  ,("5"     , "succ 4")
                  ,("6"     , "succ 5")
                  ,("7"     , "succ 6")
                  ,("8"     , "succ 7")
                  ,("9"     , "succ 8")
                  ,("add"   , "lambda m lambda n lambda f lambda x (m f) ((n f) x)")
                  ,("mul"   , "lambda m lambda n lambda f lambda x (m (n f)) x")
                  ,("pow"   , "lambda m lambda n (n (mul m)) 1")
                  ,("zero?" , "lambda n (n (const false)) true")
                  ,("succ?" , "lambda n (n (const true)) false")

                  -- Pair a b : a -> b -> (a -> b -> c)
                  ,("pair"  , "lambda a lambda b lambda f (f a) b")
                  ,("fst"   , "lambda p p (lambda a lambda b a)")   -- flip apply tru
                  ,("snd"   , "lambda p p (lambda a lambda b b)")   -- flip apply fls

                  -- List a : (a -> b -> b) -> b -> List a -> b
                  ,("fold"    , "todo")
                  ,("null"    , "todo")
                  ,("cons"    , "todo")
                  ,("snoc"    , "todo")
                  ,("map"     , "todo")
                  ,("reverse" , "todo")
                  ,("filter"  , "todo")
                  ,("any?"    , "todo")
                  ,("all?"    , "todo")
                  ,("none?"   , "todo")
                  ]

--------------------------------------------------------------------------------

type Eval a
  = ReaderT Environment (ErrorT T.Text Identity) a

runEval :: Environment -> Eval a -> Either T.Text a
runEval env action
  = runIdentity (runErrorT (runReaderT action env))

--------------------------------------------------------------------------------

evalLexical :: Expression -> Eval Value
evalLexical (Variable x)
  = do env <- ask
       case M.lookup x env of
         Just v  -> return v
         Nothing -> throwError (T.append "undefined: " x)
evalLexical e@(Abstraction _ _)
  = do env <- ask
       return (VClosure env e)
evalLexical (Application f e)
  = do (VClosure env (Abstraction x e')) <- evalLexical f
       argument                          <- evalLexical e
       local (const (extendEnv x argument env)) (evalLexical e')

--------------------------------------------------------------------------------

evalDynamic :: Expression -> Eval Value
evalDynamic (Variable x)
  = do env <- ask
       case M.lookup x env of
         Just v  -> return v
         Nothing -> throwError (T.append "undefined: " x)
evalDynamic e@(Abstraction _ _)
  = return (VClosure emptyEnv e)
evalDynamic (Application f e)
  = do (VClosure _ (Abstraction x e')) <- evalDynamic f
       argument                        <- evalDynamic e
       local (extendEnv x argument) (evalDynamic e')
