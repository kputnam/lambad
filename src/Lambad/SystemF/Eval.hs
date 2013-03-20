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

import Data.Monoid
import Data.List            ((\\), nub)

import Lambad.SystemF.Syntax

type Id
  = T.Text

data Step a
  = Antecedent Term
  | Consequent a
  deriving (Eq, Show)

type Environment a
  = M.Map Id a

-- Lift universal quantification towards the root
normalize :: Type -> Type
normalize t@(TyVariable _)            = t
normalize (TyArrow i (TyForall a o))  = TyForall a (normalize (TyArrow i o))
normalize (TyArrow i o)               = TyArrow (normalize i) (normalize o)
normalize (TyForall a t)              = TyForall a (normalize t)

-- α-Equivalence
instance Eq Type where
  TyVariable a == TyVariable a'
    = a == a'
  TyArrow i o == TyArrow i' o'
    = normalize i == normalize i' && normalize o == normalize o'
  TyForall a t == TyForall a' t'
    = normalize t == normalize (substitute (a', TyVariable a) t')
  _ == _ = False

-- α-Equivalence
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
