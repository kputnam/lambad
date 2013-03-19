{-# LANGUAGE OverloadedStrings #-}

module Lambad.SystemF.Syntax
  ( Term(..)
  , Type(..)
  , Definition(..)
  ) where

import Prelude    hiding (unwords)
import Data.Text  hiding (reverse)
import Text.PrettyPrint

import Lambad.Pretty

type Id
  = Text

data Type                       -- τ ::=
  = TyVariable Id               --     | α
  | TyForall Id Type            --     | ∀α.τ
  | TyArrow Type Type           --     | τ → τ
  deriving (Show)

data Term                       -- e ::=
  = TmVariable Id               --     | x
  | TmApplication Term Term     --     | e e
  | TmAbstraction Id Type Term  --     | λx:τ
  | TyApplication Term Type     --     | e [τ]
  | TyAbstraction Id Term       --     | Λα.e
  deriving (Show)

data Definition
  = Definition Id Term
  deriving (Show)

instance Pretty Definition where
  pretty (Definition x e)
    = parens $ text "define" <+> text (unpack x) <+> pretty e

instance Pretty Type where
  pretty (TyVariable a) = undefined
  pretty (TyForall a t) = undefined
  pretty (TyArrow t t') = undefined

instance Pretty Term where
  pretty (TmVariable x)        = undefined
  pretty (TmApplication f a)   = undefined
  pretty (TmAbstraction x t e) = undefined
  pretty (TyApplication f t)   = undefined
  pretty (TyAbstraction a e)   = undefined

