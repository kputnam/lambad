{-# LANGUAGE OverloadedStrings #-}

module Lambad.SystemF.Syntax
  ( Term(..)
  , Type(..)
  , Definition(..)
  ) where

import Prelude    hiding (unwords)
import Data.Text  hiding (reverse)
import Text.PrettyPrint hiding ((<>))
import Data.Monoid ((<>))

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
  | TmAbstraction Id Type Term  --     | λx:τ.e
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
  pretty (TyVariable a) = text (unpack a)
  pretty (TyForall a t) = text (unpack ("∀" <> a <> ".")) <+> pretty t
  pretty (TyArrow i o)  = pretty i <+> text "→" <+> pretty o

instance Pretty Term where
  pretty (TmVariable x)        = text (unpack x)
  pretty (TmApplication f a)   = parens (pretty f) <+> parens (pretty a)
  pretty (TyApplication f t)   = parens (pretty f) <+> brackets (pretty t)

  pretty e@(TmAbstraction _ _ _)
    = text "λ" <> collapse e ""
    where
      collapse (TmAbstraction x t b) s
                   = text (unpack (s <> x <> ":")) <> pretty t <> collapse b " "
      collapse b _ = text "." <+> pretty b

  pretty (TyAbstraction a e)
    = text "Λ" <> text (unpack (unwords vars))
               <> (text "." <+> pretty (snd inner))
    where
      vars  = reverse (fst inner)
      inner = collapse ([a], e)
      collapse (as, TyAbstraction a' e') = collapse (a':as, e')
      collapse (as, e')                  = (as, e')
