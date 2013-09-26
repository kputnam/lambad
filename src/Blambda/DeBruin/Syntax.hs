module Blambda.DeBruijn.Syntax
  ( Term(..)
  , Declaration(..)
  ) where

import Blambda.Bound

data Term a
  = Var a
  | App (Term a) (Term a)
  | Lam (Scope () Term a) -- Bound variables have unit type (instead of
                          -- Nat) because we derive binder location from
                          -- how @a@ was instantiated
  deriving (Show, Eq)

-- Substitution on (free) variables
instance Monad Term where
  return        = Var
  Var a   >>= f = f a
  App g x >>= f = App (g >>= f) (x >>= f)
  Lam e   >>= f = Lam (e >>>= f)
