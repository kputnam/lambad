{-# LANGUAGE OverloadedStrings #-}

module Lambad.DeBruijn.Eval
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
  , Eval
  , Step(..)
  , Environment
  ) where

import qualified Data.Map  as M
import qualified Data.Text as T

import Control.Arrow (second)
import Control.Applicative
import Data.Maybe

import Control.Monad.Reader
import Control.Monad.Writer

import Lambad.Eval
import Lambad.Misc
import Lambad.DeBruijn.Syntax

--
--------------------------------------------------------------------------------

buildEnv :: (Term -> Eval a) -> [Declaration] -> Either T.Text (Environment a)
buildEnv interpreter = be emptyEnv
  where
    be env [] = Right env
    be env (Declaration x expr:xs)
      = let (res, _) = runEval env (interpreter expr)
        in do val <- res
              be (extendEnv x val env) xs


trace :: (Term -> Eval Term) -> Term -> Eval Term
trace r e = antecedent e *> r e >>= liftA2 (*>) consequent return
  where antecedent = tell . (:[]) . Antecedent
        consequent = tell . (:[]) . Consequent

--
--------------------------------------------------------------------------------

appHelper :: Term -> Term -> Term
appHelper arg body = shift (-1) $ substitute (0, shift 1 arg) body
  where
    substitute :: (Int, Term) -> Term -> Term
    substitute (n, v)
      = mapTerm (\c m -> if m == c + n then shift c v else BoundVariable m)

    shift :: Int -> Term -> Term
    shift d
      = mapTerm (\c m -> BoundVariable $ if m >= c then m + d else m)

    mapTerm :: (Int -> Int -> Term) -> Term -> Term
    mapTerm f = walk 0
      where
        walk _ e@(FreeVariable _) = e
        walk c (Application f a)  = Application (walk c f) (walk c a)
        walk c (Abstraction b)    = Abstraction $ walk (c + 1) b
        walk c (BoundVariable n') = f c n'

-- Evaluate two expressions and test equivalence
eqHelper :: (Term, Term) -> Eval Term -> Eval Term
eqHelper (a, b) other
  = do inLambda <- asks snd
       if inLambda
          then other
          else alphaEq hybridApplicative a b
  where
    alphaEq :: Eq a => (Term -> Eval a) -> Term -> Term -> Eval a
    alphaEq interpreter a b
      = do a' <- interpreter a
           b' <- interpreter b
           interpreter (if a' == b' then true else false)
      where
        true   = Abstraction (Abstraction (BoundVariable 1))
        false  = Abstraction (Abstraction (BoundVariable 0))

-- Reduce (λx. f x) to f
etaReduce :: Term -> Term
etaReduce e@(Abstraction (Application f (BoundVariable 0)))
  = fromMaybe e (walk 0 f)
  where
    walk _ e@(FreeVariable _) = Just e
    walk n (Application f a)  = Application <$> walk n f <*> walk n a
    walk n (Abstraction b)    = Abstraction <$> walk (n+1) b
    walk n (BoundVariable m)
      | n == m    = Nothing
      | otherwise = Just (BoundVariable (m-1))
etaReduce e = e

-- Indicate we're "evaluating under lambda" when evaluating the given argument
inLambda :: Eval a -> Eval a
inLambda = local $ second (const True)

--
--------------------------------------------------------------------------------

-- Reduce to weak head normal form
callByName :: Term -> Eval Term
callByName = bn
  where
    bn = trace bn'
    bn' e@(FreeVariable x)  = M.findWithDefault e x <$> asks fst
    bn' e@(BoundVariable _) = pure e
    bn' e@(Abstraction _)   = pure (etaReduce e)
    bn' (Application f a)   = applyM2 app (bn f) (pure a)
    app (Abstraction b) a   = bn $ appHelper a b
    app f@(Application (FreeVariable "=") a) b
                            = eqHelper (a, b) (pure $ Application f b)
    app f a                 = pure $ Application f a

-- Reduce to normal form
normalOrder :: Term -> Eval Term
normalOrder = no
  where
    no = trace no'
    no' e@(BoundVariable _) = pure e
    no' e@(FreeVariable x)  = M.findWithDefault e x <$> asks fst
    no' (Abstraction b)     = inLambda $ etaReduce . Abstraction <$> no b
    no' (Application f a)   = applyM2 app (no f) (pure a)
    app (Abstraction b) a   = no $ appHelper a b
    app f@(Application (FreeVariable "=") a) b
                            = eqHelper (a, b) (Application <$> no f <*> no b)
    app f a                 = Application <$> no f <*> no a

-- Reduce to weak normal form
callByValue :: Term -> Eval Term
callByValue = bv
  where
    bv = trace bv'
    bv' e@(BoundVariable _) = pure e
    bv' e@(FreeVariable x)  = M.findWithDefault e x <$> asks fst
    bv' e@(Abstraction _)   = pure (etaReduce e)
    bv' (Application f a)   = applyM2 app (bv f) (bv a)
    app (Abstraction b) a   = bv $ appHelper a b
    app f@(Application (FreeVariable "=") a) b
                            = eqHelper (a, b) (pure $ Application f b)
    app f a                 = pure $ Application f a

-- Reduce to normal form
applicativeOrder :: Term -> Eval Term
applicativeOrder = ao
  where
    ao = trace ao'
    ao' e@(BoundVariable _) = pure e
    ao' e@(FreeVariable x)  = M.findWithDefault e x <$> asks fst
    ao' (Abstraction b)     = inLambda $ etaReduce . Abstraction <$> ao b
    ao' (Application f a)   = applyM2 app (ao f) (ao a)
    app (Abstraction b) a   = ao $ appHelper a b
    app f@(Application (FreeVariable "=") a) b
                            = eqHelper (a, b) (pure $ Application f b)
    app f a                 = pure $ Application f a

-- Reduce to normal form
hybridApplicative :: Term -> Eval Term
hybridApplicative = ha
  where
    bv = callByValue
    ha = trace ha'
    ha' e@(BoundVariable _) = pure e
    ha' e@(FreeVariable x)  = M.findWithDefault e x <$> asks fst
    ha' (Abstraction b)     = inLambda $ etaReduce . Abstraction <$> ha b
    ha' (Application f a)   = applyM2 app (bv f) (ha a)
    app (Abstraction b) a   = ha $ appHelper a b
    app f@(Application (FreeVariable "=") a) b
                            = eqHelper (a, b) (Application <$> ha f <*> pure b)
    app f a                 = Application <$> ha f <*> pure a

-- Reduce to head normal form
headSpine :: Term -> Eval Term
headSpine = he
  where
    he = trace he'
    he' e@(BoundVariable _) = pure e
    he' e@(FreeVariable x)  = M.findWithDefault e x <$> asks fst
    he' (Abstraction b)     = inLambda $ etaReduce . Abstraction <$> he b
    he' (Application f a)   = applyM2 app (he f) (pure a)
    app (Abstraction b) a   = he $ appHelper a b
    app f@(Application (FreeVariable "=") a) b
                            = eqHelper (a, b) (pure $ Application f b)
    app f a                 = pure $ Application f a

-- Reduce to normal form
hybridNormal :: Term -> Eval Term
hybridNormal = hn
  where
    he = headSpine
    hn = trace hn'
    hn' e@(BoundVariable _) = pure e
    hn' e@(FreeVariable x)  = M.findWithDefault e x <$> asks fst
    hn' (Abstraction b)     = inLambda $ etaReduce . Abstraction <$> hn b
    hn' (Application f a)   = applyM2 app (he f) (pure a)
    app (Abstraction b) a   = hn $ appHelper a b
    app f@(Application (FreeVariable "=") a) b
                            = eqHelper (a, b) (Application <$> hn f <*> hn b)
    app f a                 = Application <$> hn f <*> hn a
