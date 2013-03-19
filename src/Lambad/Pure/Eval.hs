{-# LANGUAGE OverloadedStrings #-}

module Lambad.Pure.Eval
  ( emptyEnv
  , buildEnv
  , extendEnv
  , runEval
  , callByName
  , normalOrder
  , callByValue
  , applicativeOrder
  , hybridApplicative
  , headSpine
  , hybridNormal
  , renderTrace
  , Eval(..)
  , Step(..)
  , Environment
  ) where

import qualified Data.Map  as M
import qualified Data.Text as T

import Data.List            ((\\), nub)
import Control.Arrow        (second)
import Control.Applicative

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Error

import Lambad.Misc
import Lambad.Pretty
import Lambad.Pure.Syntax

instance Error T.Text where
  noMsg   = ""
  strMsg  = T.pack

type Id
  = T.Text

data Step a
  = Antecedent Expression
  | Consequent a
  deriving (Eq, Show)

type Environment a
  = M.Map Id a

-- α-Equivalence
instance Eq Expression where
  Variable x == Variable y           = x == y
  Application f a == Application g b = f == g && a == b
  Abstraction x f == Abstraction y g
    | x == y    = f == g
    | otherwise = f == substitute (y, Variable x) g
  _ == _ = False

--------------------------------------------------------------------------------

type Eval a
  = ReaderT (Environment a, Bool) (ErrorT T.Text (WriterT [Step a] Identity)) a

runEval :: Environment a -> Eval a -> (Either T.Text a, [Step a])
runEval env action
  = runIdentity $ runWriterT $ runErrorT $ runReaderT action (env, False)

renderTrace :: Pretty a => [Step a] -> T.Text
renderTrace = T.unlines . map trace . indentTrace
  where
    trace (n, e)        = T.replicate n "  " <> step e
    step (Antecedent e) = ">> " <> renderText e
    step (Consequent e) = "=> " <> renderText e

indentTrace :: [Step a] -> [(Int, Step a)]
indentTrace = reverse . walk' 0 []
  where
    walk' _ r []                  = r
    walk' n r (Antecedent x:xs) = walk' (n + 1) ((n,     Antecedent x):r) xs
    walk' n r (Consequent x:xs) = walk' (n - 1) ((n - 1, Consequent x):r) xs

--------------------------------------------------------------------------------

emptyEnv :: Environment a
emptyEnv = M.empty

extendEnv :: Id -> a -> Environment a -> Environment a
extendEnv = M.insert

buildEnv :: (Expression -> Eval a) -> [Definition] -> Either T.Text (Environment a)
buildEnv interpreter = be emptyEnv
  where
    be env [] = Right env
    be env (Definition x expr:xs)
      = let (res, _) = runEval env (interpreter expr)
        in do val <- res
              be (extendEnv x val env) xs

freevars :: Expression -> [Id]
freevars (Variable x)      = [x]
freevars (Application e f) = nub (freevars e ++ freevars f)
freevars (Abstraction x e) = freevars e \\ [x]

-- substitute (x, a) b = b[a/x] = (λx.b) a
substitute :: (Id, Expression) -> Expression -> Expression
substitute s (Application f a)
  = Application (substitute s f) (substitute s a)
substitute (x, v) e@(Variable x')
  | x == x'   = v
  | otherwise = e
substitute s@(x, v) (Abstraction y b)
  | x == y                 = Abstraction y b
  | y `notElem` freevars v = Abstraction y (substitute s b)
  | otherwise = let y' = freshvar y (freevars v)
                    b' = substitute (y, Variable y') b
                 in substitute s (Abstraction y' b')
  where
    freshvar x xs
      | x `elem` xs = freshvar (x <> "'") xs
      | otherwise   = x

eqHelper :: (Expression, Expression) -> Eval Expression -> Eval Expression
eqHelper (a, b) other
  = do inLambda <- asks snd
       if inLambda
          then other
          else alphaEq hybridApplicative a b
  where
    alphaEq :: Eq a => (Expression -> Eval a) -> Expression -> Expression -> Eval a
    alphaEq interpreter a b
      = do a' <- interpreter a
           b' <- interpreter b
           interpreter (if a' == b' then true else false)
      where
        true   = Abstraction "t" (Abstraction "f" (Variable "t"))
        false  = Abstraction "t" (Abstraction "f" (Variable "f"))

etaReduce :: Expression -> Expression
etaReduce e@(Abstraction x (Application f y))
  | Variable x == y && x `notElem` freevars f = f
  | otherwise = e
etaReduce e   = e

inLambda :: Eval a -> Eval a
inLambda = local $ second (const True)

trace :: (Expression -> Eval a) -> Expression -> Eval a
trace r e = antecedent e *> r e >>= liftA2 (*>) consequent return
  where antecedent = tell . (:[]) . Antecedent
        consequent = tell . (:[]) . Consequent

--------------------------------------------------------------------------------

-- Reduce to weak head normal form
callByName :: Expression -> Eval Expression
callByName = bn
  where
    bn = trace bn'
    bn' e@(Variable x)      = M.findWithDefault e x <$> asks fst
    bn' e@(Abstraction _ _) = pure (etaReduce e)
    bn' (Application f a)   = applyM2 app (bn f) (pure a)
    app (Abstraction x b) a = bn $ substitute (x, a) b
    app f@(Application (Variable "=") a) b
                            = eqHelper (a, b) (pure $ Application f b)
    app f a                 = pure $ Application f a

-- Reduce to normal form
normalOrder :: Expression -> Eval Expression
normalOrder = no
  where
    no = trace no'
    no' e@(Variable x)      = M.findWithDefault e x <$> asks fst
    no' (Abstraction x b)   = inLambda $ etaReduce . Abstraction x <$> no b
    no' (Application f a)   = applyM2 app (no f) (pure a)
    app (Abstraction x b) a = no $ substitute (x, a) b
    app f@(Application (Variable "=") a) b
                            = eqHelper (a, b) (Application <$> no f <*> no b)
    app f a                 = Application <$> no f <*> no a

-- Reduce to weak normal form
callByValue :: Expression -> Eval Expression
callByValue = bv
  where
    bv = trace bv'
    bv' e@(Variable x)      = M.findWithDefault e x <$> asks fst
    bv' e@(Abstraction _ _) = pure (etaReduce e)
    bv' (Application f a)   = applyM2 app (bv f) (bv a)
    app (Abstraction x b) a = bv $ substitute (x, a) b
    app f@(Application (Variable "=") a) b
                            = eqHelper (a, b) (pure $ Application f b)
    app f a                 = pure $ Application f a

-- Reduce to normal form
applicativeOrder :: Expression -> Eval Expression
applicativeOrder = ao
  where
    ao = trace ao'
    ao' e@(Variable x)      = M.findWithDefault e x <$> asks fst
    ao' (Abstraction x b)   = inLambda $ etaReduce . Abstraction x <$> ao b
    ao' (Application f a)   = applyM2 app (ao f) (ao a)
    app (Abstraction x b) a = ao $ substitute (x, a) b
    app f@(Application (Variable "=") a) b
                            = eqHelper (a, b) (pure $ Application f b)
    app f a                 = pure $ Application f a

-- Reduce to normal form
hybridApplicative :: Expression -> Eval Expression
hybridApplicative = ha
  where
    bv = callByValue
    ha = trace ha'
    ha' e@(Variable x)      = M.findWithDefault e x <$> asks fst
    ha' (Abstraction x b)   = inLambda $ etaReduce . Abstraction x <$> ha b
    ha' (Application f a)   = applyM2 app (bv f) (ha a)
    app (Abstraction x b) a = ha $ substitute (x, a) b
    app f@(Application (Variable "=") a) b
                            = eqHelper (a, b) (Application <$> ha f <*> pure b)
    app f a                 = Application <$> ha f <*> pure a

-- Reduce to head normal form
headSpine :: Expression -> Eval Expression
headSpine = he
  where
    he = trace he'
    he' e@(Variable x)      = M.findWithDefault e x <$> asks fst
    he' (Abstraction x b)   = inLambda $ etaReduce . Abstraction x <$> he b
    he' (Application f a)   = applyM2 app (he f) (pure a)
    app (Abstraction x b) a = he $ substitute (x, a) b
    app f@(Application (Variable "=") a) b
                            = eqHelper (a, b) (pure $ Application f b)
    app f a                 = pure $ Application f a

-- Reduce to normal form
hybridNormal :: Expression -> Eval Expression
hybridNormal = hn
  where
    he = headSpine
    hn = trace hn'
    hn' e@(Variable x)      = M.findWithDefault e x <$> asks fst
    hn' (Abstraction x b)   = inLambda $ etaReduce . Abstraction x <$> hn b
    hn' (Application f a)   = applyM2 app (he f) (pure a)
    app (Abstraction x b) a = hn $ substitute (x, a) b
    app f@(Application (Variable "=") a) b
                            = eqHelper (a, b) (Application <$> hn f <*> hn b)
    app f a                 = Application <$> hn f <*> hn a
