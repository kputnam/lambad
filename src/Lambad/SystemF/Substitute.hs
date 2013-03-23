{-# LANGUAGE OverloadedStrings,
             MultiParamTypeClasses #-}

module Lambad.SystemF.Substitute
  ( Substitute(..)
  , Free(..)
  , freshvar
  ) where

import Data.Text (Text)
import Data.List ((\\), nub)
import Data.Monoid
import Lambad.SystemF.Syntax

type Id
  = Text

class Substitute a b where
  freevars   :: a -> Free b
  substitute :: (Id, b) -> a -> a

-- Free variables bound to values of type 'b'
newtype Free a = Free { getFree :: [Id] }

-- Generate an identifier similar to x, not contained in xs
freshvar :: Id -> [Id] -> Id
freshvar x xs
  | x `elem` xs = freshvar (x <> "'") xs
  | otherwise   = x

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
