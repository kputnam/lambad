{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Lambad.Eval
  ( emptyEnv
  , defaultEnv
  , runEval
  , defaultEval
  , callByName
  , normalOrder
  , callByValue
  , applicativeOrder
  , hybridApplicative
  , headSpine
  , hybridNormal
  ) where

import qualified Data.Map  as M
import qualified Data.Text as T

import Data.List            ((\\), nub)
import Data.Functor         ((<$>))
import Data.Attoparsec.Text (parseOnly)
import Control.Arrow        (second)
import Control.Applicative  ((<*>))

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Error

import Language.Lambad.Misc
import Language.Lambad.Syntax
import Language.Lambad.Parser
import Language.Lambad.Pretty

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

emptyEnv ∷ Environment
emptyEnv = M.empty

extendEnv ∷ Id → Value → Environment → Environment
extendEnv = M.insert

-- We're using majiks here, because we want each top-level definition to
-- be able to reference any other top-level definition (including itself)
-- for the sake of convenience. This saves from having to sort definitions
-- TODO: buildEnv ∷ [Declaration] → Environment
defaultEnv ∷ Environment
defaultEnv
  = M.fromList $ map (second closure) definitions
  where
    parse s     = T.pack `lmap` parseOnly parseExpr s
    eval s      = runEval defaultEnv =<< defaultEval `fmap` parse s
    closure s   = either (error . T.unpack) id $ eval s
    definitions = [("id"    , "lambda x. x")
                  ,("apply" , "lambda f. lambda x. f x") -- id
                  ,("const" , "lambda x. lambda y. x")
                  ,("flip"  , "lambda f. lambda a. lambda b. f b a")
                  ,("S"     , "lambda x. lambda y. lambda z. x z (y z)") -- reader
                  ,("K"     , "lambda x. lambda y. lambda x") -- const
                  ,("I"     , "lambda x. x") -- id
                  ,("Y"     , "(lambda f. lambda x. f (x x)) (lambda f. lambda x. f (x x))")
                  ,("Z"     , "(lambda f. lambda x. f (lambda y. (x x) y)) (lambda f. lambda x. f (lambda y. (x x) y))")
                  
                  -- Bool : a → a → a
                  ,("if"    , "lambda x. lambda t. lambda f. (x t) f")
                  ,("not"   , "lambda x. lambda t. lambda f. (x f) t")
                  ,("true"  , "lambda t. lambda f. t") -- const
                  ,("false" , "lambda t. lambda f. f") -- flip const
                  ,("or"    , "lambda x. lambda y. (x x) y")
                  ,("and"   , "lambda x. lambda y. (x y) x")
                  ,("xor"   , "todo")

                  -- Nat : (a → a) → a → a
                  ,("succ"  , "lambda n. lambda f. lambda x. f (n f x)")
                  ,("0"     , "lambda f. lambda x. x") -- flip const
                  ,("1"     , "succ 0")
                  ,("2"     , "succ 1")
                  ,("3"     , "succ 2")
                  ,("4"     , "succ 3")
                  ,("5"     , "succ 4")
                  ,("6"     , "succ 5")
                  ,("7"     , "succ 6")
                  ,("8"     , "succ 7")
                  ,("9"     , "succ 8")
                  ,("add"   , "lambda m. lambda n. lambda f. lambda x. (m f) ((n f) x)")
                  ,("mul"   , "lambda m. lambda n. lambda f. lambda x. (m (n f)) x")
                  ,("pow"   , "lambda m. lambda n. (n (mul m)) 1")
                  ,("zero?" , "lambda n. (n (const false)) true")
                  ,("succ?" , "lambda n. (n (const true)) false")

                  -- Pair a b : a → b → (a → b → c)
                  ,("pair"  , "lambda a. lambda b. lambda f. (f a) b")
                  ,("fst"   , "lambda p. p (lambda a. lambda b. a)")   -- flip apply tru
                  ,("snd"   , "lambda p. p (lambda a. lambda b. b)")   -- flip apply fls

                  -- List a : (a → b → b) → b → List a → b
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

runEval ∷ Environment → Eval a → Either T.Text a
runEval env action
  = runIdentity (runErrorT (runReaderT action env))

--------------------------------------------------------------------------------

defaultEval ∷ Expression → Eval Value
defaultEval (Variable x)
  = do env <- ask
       case M.lookup x env of
         Just v  → return v
         Nothing → throwError (T.append "undefined: " x)
defaultEval e@(Abstraction _ _)
  = do env <- ask
       return (VClosure env e)
defaultEval (Application f e)
  = do (VClosure env (Abstraction x e')) <- defaultEval f
       argument                          <- defaultEval e
       local (const (extendEnv x argument env)) (defaultEval e')

--------------------------------------------------------------------------------

-- itu.dk/people/sestoft/papers/sestoft-lamreduce.pdf
--                                                   
--                                    Reduce under λ 
--       +------------------+-----------------------+
-- Strict|                Y |                     N |
-- +-----+------------------+-----------------------+
-- |   Y |      Normal form |      Weak normal form |
-- |     |  E ∷= λx.E , x E |       E ∷= λx.e , x E |
-- |     |                  |                       |
-- |     |   ao, no, ha, hn |                    bv |
-- +-----+------------------+-----------------------+
-- |   N | Head normal form | Weak head normal form |
-- |     |  E ∷= λx.E , x e |       E ∷= λx.e , x e |
-- |     |                  |                       |
-- |     |               he |                    bn |
-- +-----+------------------+-----------------------+
--                               e ∷= x | λx.e | e e 

substitute ∷ (Id, Expression) → Expression → Expression
substitute s (Application e f)
  = Application (substitute s e) (substitute s f)
substitute (x, v) e@(Variable x')
  | x == x'   = v
  | otherwise = e
substitute s@(_, v) (Abstraction x b)
  | x `notElem` freevars v = Abstraction x (substitute s b)
  | otherwise = let x' = freshvar x (freevars v)
                    b' = substitute (x, Variable x') b
                 in Abstraction x' (substitute (x', v) b')
  where
    freshvar x xs
      | x `elem` xs = freshvar (T.append x "'") xs
      | otherwise   = x
    freevars (Variable x)      = [x]
    freevars (Application e f) = nub (freevars e ++ freevars f)
    freevars (Abstraction x e) = freevars e \\ [x]

--------------------------------------------------------------------------------

callByName ∷ Expression → Eval Expression
callByName = bn
  where
    bn e@(Variable _)      = return e
    bn e@(Abstraction _ _) = return e
    bn (Application (Abstraction x b) a)
      = bn (substitute (x, a) b)
    bn e@(Application _ _) = return e

normalOrder ∷ Expression → Eval Expression
normalOrder = no
  where
    no e@(Variable _)    = return e
    no (Abstraction x b) = Abstraction x <$> no b
    no (Application f a)
      = do f' <- callByName f
           case f' of
             Abstraction x b → no (substitute (x, a) b)
             _               → Application <$> no f' <*> no a

callByValue ∷ Expression → Eval Expression
callByValue = bv
  where
    bv e@(Variable _)      = return e
    bv e@(Abstraction _ _) = return e
    bv (Application f a)
      = do f' <- bv f
           a' <- bv a
           case f' of
             Abstraction x b → bv (substitute (x, a') b)
             _               → return (Application f' a')

applicativeOrder ∷ Expression → Eval Expression
applicativeOrder = ao
  where
    ao e@(Variable _)    = return e
    ao (Abstraction x b) = Abstraction x <$> ao b
    ao (Application f a)
      = do f' <- ao f
           a' <- ao a
           case f' of
             Abstraction x b → ao (substitute (x, a') b)
             _               → return (Application f' a')

hybridApplicative ∷ Expression → Eval Expression
hybridApplicative = ha
  where
    ha e@(Variable _)    = return e
    ha (Abstraction x b) = Abstraction x <$> ha b
    ha (Application f a)
      = do f' <- ha f
           a' <- ha a
           case f' of
             Abstraction x b → ha (substitute (x, a') b)
             _               → flip Application a' <$> ha f'

headSpine ∷ Expression → Eval Expression
headSpine = he
  where
    he e@(Variable _)    = return e
    he (Abstraction x b) = Abstraction x <$> he b
    he (Application f a)
      = do f' <- he f
           case f' of
             Abstraction x b → he (substitute (x, a) b)
             _               → return (Application f' a)

hybridNormal ∷ Expression → Eval Expression
hybridNormal = hn
  where
    hn e@(Variable _)    = return e
    hn (Abstraction x b) = Abstraction x <$> hn b
    hn (Application f a)
      = do f' <- headSpine f
           case f' of
             Abstraction x b → hn (substitute (x, a) b)
             _               → Application <$> hn f' <*> hn a

--------------------------------------------------------------------------------
