{-# LANGUAGE OverloadedStrings #-}

module Language.Lambad.Eval
  ( evalStrict
  , evalLazy
  , evalDynamic
  , emptyEnv
  , defaultEnv
  , runEval
  ) where

import Prelude hiding (lookup)

import qualified Data.Map  as M
import qualified Data.Text as T

import Control.Arrow
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Error

import Language.Lambad.Syntax
import Language.Lambad.Parser
import Data.Attoparsec.Text (parseOnly)

type Id
  = T.Text

--------------------------------------------------------------------------------

data Value
  = VClosure Environment Id Expression

instance Show Value where
  show (VClosure _ x e) = "lambda " ++ T.unpack x ++ " " ++ show e

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
-- and also sneaks in general recursion.
defaultEnv :: Environment
defaultEnv
  = M.fromList $ map (second closure) definitions
  where
    closure s   = case parseOnly parseExpr s of
                    Right e -> evalStrict e
                    Right (Abstraction x e) -> VClosure defaultEnv x e
                    Right e                 -> VClosure defaultEnv "_" e
    definitions = [("id"    , "lambda x x")
                  ,("fix"   , "(lambda f lambda x f (x x)) (lambda f lambda x f (x x))")
                  ,("apply" , "lambda f lambda x f x")              -- id
                  ,("const" , "lambda x lambda _ x")
                  ,("flip"  , "lambda f lambda a lambda b (f b) a")
                  
                  -- Bool : a -> a -> a
                  ,("if"    , "lambda x lambda t lambda f (x t) f") -- id
                  ,("not"   , "lambda x lambda t lambda f (x f) t") -- flip
                  ,("true"  , "lambda t lambda f t")                -- const
                  ,("false" , "lambda t lambda f f")                -- flip const
                  ,("or"    , "lambda x lambda y (x x) y")
                  ,("and"   , "lambda x lambda y (x y) x")
                  ,("xor"   , "todo")

                  -- Nat : (a -> a) -> a -> a
                  ,("succ"  , "lambda f lambda x f x")              -- id
                  ,("0"     , "lambda f lambda x x")                -- flip const
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

evalStrict :: Expression -> Eval Value
evalStrict (Variable x)
  = do env <- ask
       case M.lookup x env of
         Just v  -> evalStrict v
         Nothing -> throwError (T.append "undefined: " x)
evalStrict (Abstraction x e)
  = do env <- ask
       return (VClosure env x e)
evalStrict (Application f e)
  = do (VClosure env x e') <- evalStrict f
       argument            <- evalStrict e
       local (const (extendEnv x argument env)) (evalStrict e')

--------------------------------------------------------------------------------

evalLazy :: Expression -> Eval Value
evalLazy (Variable x)
  = do env <- ask
       case M.lookup x env of
         Just (VClosure env' _ e) -> local (const env') (evalLazy e)
         Nothing                  -> throwError (T.append "undefined: " x)
evalLazy e@(Abstraction _ _)
  = do env <- ask
       return (VClosure env "_" e)
evalLazy (Application f e)
  = undefined f e

--------------------------------------------------------------------------------

evalDynamic :: Expression -> Eval Value
evalDynamic (Variable x)
  = do env <- ask
       case M.lookup x env of
         Just v  -> return v
         Nothing -> throwError (T.append "undefined: " x)
evalDynamic (Abstraction x e)
  = return (VClosure emptyEnv x e)
evalDynamic (Application f e)
  = do (VClosure _ x e') <- evalDynamic f
       argument          <- evalDynamic e
       local (extendEnv x argument) (evalDynamic e')
