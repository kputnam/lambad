{-# LANGUAGE OverloadedStrings,
             MultiParamTypeClasses #-}

module Lambad.SystemF.Eval
  ( emptyEnv
  , buildEnv
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

import Control.Arrow        (second)
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Writer

import Lambad.Eval
import Lambad.Misc
import Lambad.SystemF.Syntax
import Lambad.SystemF.Substitute

type Id
  = T.Text

trace :: (Term -> Eval Term) -> Term -> Eval Term
trace r e = antecedent e *> r e >>= liftA2 (*>) consequent return
  where antecedent = tell . (:[]) . Antecedent
        consequent = tell . (:[]) . Consequent

buildEnv :: (Term -> Eval a) -> [Definition] -> Either T.Text (Environment a)
buildEnv interpreter = be emptyEnv
  where
    be env [] = Right env
    be env (Definition x expr:xs)
      = let (res, _) = runEval env (interpreter expr)
        in do val <- res
              be (extendEnv x val env) xs

-- Lift universal quantification towards the root
normalize :: Type -> Type
normalize t@(TyVariable _)            = t
normalize (TyArrow i (TyForall a o))  = TyForall a (normalize (TyArrow i o))
normalize (TyArrow i o)               = TyArrow (normalize i) (normalize o)
normalize (TyForall a t)              = TyForall a (normalize t)

-- α-Equivalence on types
instance Eq Type where
  TyVariable a == TyVariable a'
    = a == a'
  TyArrow i o == TyArrow i' o'
    = normalize i == normalize i' && normalize o == normalize o'
  TyForall a t == TyForall a' t'
    = normalize t == normalize (substitute (a', TyVariable a) t')
  _ == _ = False

-- α-Equivalence on terms
instance Eq Term where
  TmVariable x == TmVariable y
    = x == y
  TmApplication f a == TmApplication f' a'
    = f == f' && a == a'
  TmAbstraction x t e == TmAbstraction x' t' e'
    = t == t' && e == substitute (x', TmVariable x) e'
  TyApplication e t == TyApplication e' t'
    = e == e' && t == t'
  TyAbstraction a e == TyAbstraction a' e'
    = a == a' && e == e'
  _ == _ = False

--
--------------------------------------------------------------------------------

-- Evaluate two expressions and test alpha-equivalence
eqHelper :: (Term, Term) -> Eval Term -> Eval Term
eqHelper (a, b) other
  = do inLambda <- asks snd
       if inLambda
          then other
          else alphaEq hybridApplicative a b
  where
    alphaEq :: Eq a => (Term -> Eval a) -> Term -> Term -> Eval a
    alphaEq interpreter t s
      = do t' <- interpreter t
           s' <- interpreter s
           interpreter (if t' == s' then true else false)
      where
        true   = undefined -- TmAbstraction "t" (TmAbstraction "f" (TmVariable "t"))
        false  = undefined -- TmAbstraction "t" (TmAbstraction "f" (TmVariable "f"))

-- Reduce (λx:τ. f x) to f and (Λα.e α) to e
etaReduce :: Term -> Term
etaReduce e@(TmAbstraction x _ (TmApplication f y))
  | TmVariable x == y && x `notElem` getFree (freevars f :: Free Term) = f
  | otherwise = e
etaReduce e@(TyAbstraction a (TyApplication f t))
  | TyVariable a == t && a `notElem` getFree (freevars e :: Free Type) = f
  | otherwise = e
etaReduce e   = e

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
    bn' e@(TmVariable x)        = M.findWithDefault e x <$> asks fst
    bn' e@(TyAbstraction _ _)   = pure (etaReduce e)
    bn' e@(TmAbstraction _ _ _) = pure (etaReduce e)
    bn' (TmApplication f a)     = applyM2 app (bn f) (pure a)
    bn' (TyApplication e t)     = applyM2 apt (bn e) (pure t)

    apt (TyAbstraction a e) t   = bn $ substitute (a, t) e
    apt e t                     = pure $ TyApplication e t

    app (TmAbstraction x _ b) a = bn $ substitute (x, a) b
    app f@(TmApplication (TmVariable "=") a) b
                                = eqHelper (a, b) (pure $ TmApplication f b)
    app f a                     = pure $ TmApplication f a

-- Reduce to normal form
normalOrder :: Term -> Eval Term
normalOrder = no
  where
    no = trace no'
    no' e@(TmVariable x)        = M.findWithDefault e x <$> asks fst
    no' (TyAbstraction a e)     = inLambda $ etaReduce . TyAbstraction a <$> no e
    no' (TmAbstraction x t b)   = inLambda $ etaReduce . TmAbstraction x t <$> no b
    no' (TmApplication f a)     = applyM2 app (no f) (pure a)
    no' (TyApplication e t)     = applyM2 apt (no e) (pure t)

    apt (TyAbstraction a e) t   = no $ substitute (a, t) e
    apt e t                     = pure $ TyApplication e t

    app (TmAbstraction x _ b) a = no $ substitute (x, a) b
    app f@(TmApplication (TmVariable "=") a) b
                                = eqHelper (a, b) (TmApplication <$> no f <*> no b)
    app f a                     = TmApplication <$> no f <*> no a

-- Reduce to weak normal form
callByValue :: Term -> Eval Term
callByValue = bv
  where
    bv = trace bv'
    bv' e@(TmVariable x)        = M.findWithDefault e x <$> asks fst
    bv' e@(TyAbstraction _ _)   = pure (etaReduce e)
    bv' e@(TmAbstraction _ _ _) = pure (etaReduce e)
    bv' (TmApplication f a)     = applyM2 app (bv f) (bv a)
    bv' (TyApplication e t)     = applyM2 apt (bv e) (pure t)

    apt (TyAbstraction a e) t   = bv $ substitute (a, t) e
    apt e t                     = pure $ TyApplication e t

    app (TmAbstraction x _ b) a = bv $ substitute (x, a) b
    app f@(TmApplication (TmVariable "=") a) b
                                = eqHelper (a, b) (pure $ TmApplication f b)
    app f a                     = pure $ TmApplication f a

-- Reduce to normal form
applicativeOrder :: Term -> Eval Term
applicativeOrder = ao
  where
    ao = trace ao'
    ao' e@(TmVariable x)        = M.findWithDefault e x <$> asks fst
    ao' (TyAbstraction a e)     = inLambda $ etaReduce . TyAbstraction a <$> ao e
    ao' (TmAbstraction x t b)   = inLambda $ etaReduce . TmAbstraction x t <$> ao b
    ao' (TmApplication f a)     = applyM2 app (ao f) (ao a)
    ao' (TyApplication e t)     = applyM2 apt (ao e) (pure t)

    apt (TyAbstraction a e) t   = ao $ substitute (a, t) e
    apt e t                     = pure $ TyApplication e t

    app (TmAbstraction x _ b) a = ao $ substitute (x, a) b
    app f@(TmApplication (TmVariable "=") a) b
                                = eqHelper (a, b) (pure $ TmApplication f b)
    app f a                     = pure $ TmApplication f a

-- Reduce to normal form
hybridApplicative :: Term -> Eval Term
hybridApplicative = ha
  where
    bv = callByValue
    ha = trace ha'
    ha' e@(TmVariable x)        = M.findWithDefault e x <$> asks fst
    ha' (TyAbstraction a e)     = inLambda $ etaReduce . TyAbstraction a <$> ha e
    ha' (TmAbstraction x t b)   = inLambda $ etaReduce . TmAbstraction x t <$> ha b
    ha' (TmApplication f a)     = applyM2 app (bv f) (ha a)
    ha' (TyApplication e t)     = applyM2 apt (bv e) (pure t)

    apt (TyAbstraction a e) t   = ha $ substitute (a, t) e
    apt e t                     = pure $ TyApplication e t

    app (TmAbstraction x _ b) a = ha $ substitute (x, a) b
    app f@(TmApplication (TmVariable "=") a) b
                                = eqHelper (a, b) (TmApplication <$> ha f <*> pure b)
    app f a                     = TmApplication <$> ha f <*> pure a

-- Reduce to head normal form
headSpine :: Term -> Eval Term
headSpine = he
  where
    he = trace he'
    he' e@(TmVariable x)        = M.findWithDefault e x <$> asks fst
    he' (TyAbstraction a e)     = inLambda $ etaReduce . TyAbstraction a <$> he e
    he' (TmAbstraction x t b)   = inLambda $ etaReduce . TmAbstraction x t <$> he b
    he' (TmApplication f a)     = applyM2 app (he f) (pure a)
    he' (TyApplication e t)     = applyM2 apt (he e) (pure t)

    apt (TyAbstraction a e) t   = he $ substitute (a, t) e
    apt e t                     = pure $ TyApplication e t

    app (TmAbstraction x _ b) a = he $ substitute (x, a) b
    app f@(TmApplication (TmVariable "=") a) b
                                = eqHelper (a, b) (pure $ TmApplication f b)
    app f a                     = pure $ TmApplication f a

-- Reduce to normal form
hybridNormal :: Term -> Eval Term
hybridNormal = hn
  where
    he = headSpine
    hn = trace hn'
    hn' e@(TmVariable x)        = M.findWithDefault e x <$> asks fst
    hn' (TyAbstraction a e)     = inLambda $ etaReduce . TyAbstraction a <$> hn e
    hn' (TmAbstraction x t b)   = inLambda $ etaReduce . TmAbstraction x t <$> hn b
    hn' (TmApplication f a)     = applyM2 app (he f) (pure a)
    hn' (TyApplication e t)     = applyM2 apt (he e) (pure t)

    apt (TyAbstraction a e) t   = hn $ substitute (a, t) e
    apt e t                     = pure $ TyApplication e t

    app (TmAbstraction x _ b) a = hn $ substitute (x, a) b
    app f@(TmApplication (TmVariable "=") a) b
                                = eqHelper (a, b) (TmApplication <$> hn f <*> hn b)
    app f a                     = TmApplication <$> hn f <*> hn a
