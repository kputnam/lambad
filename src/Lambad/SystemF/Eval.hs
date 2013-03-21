{-# LANGUAGE OverloadedStrings,
             MultiParamTypeClasses #-}

module Lambad.SystemF.Eval
{-( emptyEnv
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
  )-} where

import qualified Data.Map  as M
import qualified Data.Text as T

import Data.List            ((\\), nub)
import Control.Arrow        (second)
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Writer

import Lambad.Eval
import Lambad.Misc
import Lambad.SystemF.Syntax

type Id
  = T.Text

trace :: (Term -> Eval Term) -> Term -> Eval Term
trace r e = antecedent e *> r e >>= liftA2 (*>) consequent return
  where antecedent = tell . (:[]) . Antecedent
        consequent = tell . (:[]) . Consequent

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

-- Free variables bound to values of type 'b'
newtype Free a = Free { getFree :: [Id] }

-- Generate an identifier similar to x, not contained in xs
freshvar :: Id -> [Id] -> Id
freshvar x xs
  | x `elem` xs = freshvar (x <> "'") xs
  | otherwise   = x

class Substitute a b where
  freevars   :: a -> Free b
  substitute :: (Id, b) -> a -> a

instance Substitute Type Type where
  -- Free type variables within a type expression
  freevars (TyVariable a) = Free [a]
  freevars (TyArrow i o)  = Free (nub (getFree (freevars i :: Free Type) ++
                                       getFree (freevars o :: Free Type)))
  freevars (TyForall a t) = Free (getFree (freevars t :: Free Type) \\ [a])

  -- Within a type expression, replace variables with types
  substitute (a, v) (TyVariable a')
    | a == a'   = v
    | otherwise = TyVariable a'
  substitute s (TyArrow i o)
    = TyArrow (substitute s i) (substitute s o)
  substitute s@(a, v) (TyForall b t)
    | a == b    = TyForall b t
    | otherwise = if a `notElem` free
                  then TyForall b (substitute s t)
                  else substitute s (TyForall b' t')
    where
      free = getFree (freevars v :: Free Type)
      b'   = freshvar b free
      t'   = substitute (b, TyVariable b') t

instance Substitute Term Term where
  -- Free term variables within a term expression
  freevars (TmVariable x)        = Free [x]
  freevars (TyApplication e _)   = freevars e
  freevars (TyAbstraction _ e)   = freevars e
  freevars (TmAbstraction x _ e) = Free (getFree (freevars e :: Free Term) \\ [x])
  freevars (TmApplication f a)   = Free (nub (getFree (freevars f :: Free Term) ++
                                              getFree (freevars a :: Free Term)))

  -- Within a term expression, replace variables with terms
  substitute s (TmApplication f a) = TmApplication (substitute s f) (substitute s a)
  substitute s (TyApplication e t) = TyApplication (substitute s e) t
  substitute s (TyAbstraction a e) = TyAbstraction a (substitute s e)
  substitute (x, v) (TmVariable y)
    | x == y    = v
    | otherwise = TmVariable y
  substitute s@(x, v) (TmAbstraction y t e)
    | x == y    = TmAbstraction y t e
    | otherwise = if y `notElem` free
                  then TmAbstraction y t (substitute s e)
                  else substitute s (TmAbstraction y' t e')
    where
      free = getFree (freevars v :: Free Term)
      y'   = freshvar y free
      e'   = substitute (y, TmVariable y') e

instance Substitute Term Type where
  -- Free type variables within a term expression
  freevars x
    = case x of
        (TmVariable _)        -> Free []
        (TmApplication f a)   -> union f a
        (TmAbstraction _ t e) -> union t e
        (TyApplication e t)   -> union e t
        (TyAbstraction a e)   -> Free $ getFree (freevars e :: Free Type) \\ [a]
    where union a b = Free $ nub $ getFree (freevars a :: Free Type) ++
                                   getFree (freevars b :: Free Type)

  -- Within a term expression, replace variables with types
  substitute _ e@(TmVariable _)      = e
  substitute s (TmApplication f a)   = TmApplication (substitute s f) (substitute s a)
  substitute s (TmAbstraction x t e) = TmAbstraction x (substitute s t) (substitute s e)
  substitute s (TyApplication e t)   = TyApplication (substitute s e) (substitute s t)
  substitute s@(a, v) (TyAbstraction b e)
    | a == b    = TyAbstraction b e
    | otherwise = if b `notElem` free
                  then TyAbstraction b (substitute s e)
                  else substitute s (TyAbstraction b' e')
    where
      free = getFree (freevars v :: Free Type)
      b'   = freshvar b free
      e'   = substitute (b, TyVariable b') e

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
    alphaEq interpreter a b
      = do a' <- interpreter a
           b' <- interpreter b
           interpreter (if a' == b' then true else false)
      where
        true   = undefined -- TmAbstraction "t" (TmAbstraction "f" (TmVariable "t"))
        false  = undefined -- TmAbstraction "t" (TmAbstraction "f" (TmVariable "f"))

-- Reduce (λx:τ. f x) to f
etaReduce :: Term -> Term
etaReduce e@(TmAbstraction x _ (TmApplication f y))
  | TmVariable x == y && x `notElem` getFree (freevars f :: Free Term) = f
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
    bn' e@(TmAbstraction _ _ _) = pure (etaReduce e)
    bn' (TmApplication f a)     = applyM2 app (bn f) (pure a)
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
    no' (TmAbstraction x t b)   = inLambda $ etaReduce . TmAbstraction x t <$> no b
    no' (TmApplication f a)     = applyM2 app (no f) (pure a)
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
    bv' e@(TmAbstraction _ _ _) = pure (etaReduce e)
    bv' (TmApplication f a)     = applyM2 app (bv f) (bv a)
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
    ao' (TmAbstraction x t b)   = inLambda $ etaReduce . TmAbstraction x t <$> ao b
    ao' (TmApplication f a)     = applyM2 app (ao f) (ao a)
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
    ha' (TmAbstraction x t b)   = inLambda $ etaReduce . TmAbstraction x t <$> ha b
    ha' (TmApplication f a)     = applyM2 app (bv f) (ha a)
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
    he' (TmAbstraction x t b)   = inLambda $ etaReduce . TmAbstraction x t <$> he b
    he' (TmApplication f a)     = applyM2 app (he f) (pure a)
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
    hn' (TmAbstraction x t b)   = inLambda $ etaReduce . TmAbstraction x t <$> hn b
    hn' (TmApplication f a)     = applyM2 app (he f) (pure a)
    app (TmAbstraction x _ b) a = hn $ substitute (x, a) b
    app f@(TmApplication (TmVariable "=") a) b
                                = eqHelper (a, b) (TmApplication <$> hn f <*> hn b)
    app f a                     = TmApplication <$> hn f <*> hn a
