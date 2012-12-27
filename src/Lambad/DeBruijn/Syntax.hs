{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Lambad.DeBruijn.Syntax
  ( Expression(..)
  , Declaration(..)
  ) where

import Data.Text
import Text.PrettyPrint

import Lambad.Pretty

type Id
  = Text

data Expression
  = FreeVariable Id
  | BoundVariable Int
  | Application Expression Expression
  | Abstraction Expression
  deriving (Show, Eq)

data Declaration
  = Declaration Id Expression
  deriving (Show, Eq)

instance Pretty Declaration where
  pretty (Declaration x e)
    = parens $ text "define" <+> text (unpack x) <+> pretty e

instance Pretty Expression where
  -- Render (λx.x) (λx.x) instead of λx.x λx.x,
  pretty (Application f@(Abstraction _) a@(Abstraction _))
    = parens (pretty f) <+> parens (pretty a)

  -- Render (λx.x) (e f) instead of (λx. x) e f
  pretty (Application f@(Abstraction _) a@(Application _ _))
    = parens (pretty f) <+> parens (pretty a)

  -- Render (λx.x) e instead of λx. x e
  pretty (Application f@(Abstraction _) a)
    = parens (pretty f) <+> pretty a

  -- Render f (λx.x) instead of f λ.x x
  pretty (Application f a@(Abstraction _))
    = pretty f <+> parens (pretty a)

  -- Render f (g x) instead of f g x
  pretty (Application f a@(Application _ _))
    = pretty f <+> parens (pretty a)

  pretty (Application f a)
    = pretty f <+> pretty a

  pretty (Abstraction e)
    = text "λ" <> text "." <+> pretty e

  pretty (FreeVariable x)
    = text (unpack x)

  pretty (BoundVariable n)
    = text (show n)
