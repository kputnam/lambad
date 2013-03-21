{-# LANGUAGE OverloadedStrings #-}

module Lambad.Pure.Syntax
  ( Term(..)
  , Definition(..)
  ) where

import Prelude    hiding (unwords)
import Data.Text  hiding (reverse)
import Text.PrettyPrint

import Lambad.Pretty

type Id
  = Text

data Term
  = Variable Id
  | Application Term Term
  | Abstraction Id Term
  deriving (Show)

data Definition
  = Definition Id Term
  deriving (Show)

instance Pretty Definition where
  pretty (Definition x e)
    = parens $ text "define" <+> text (unpack x) <+> pretty e

instance Pretty Term where
  -- Render (λx.x) (λx.x) instead of λx.x λx.x,
  pretty (Application f@(Abstraction _ _) x@(Abstraction _ _))
    = parens (pretty f) <+> parens (pretty x)

  -- Render (λx.x) (e f) instead of (λx. x) e f
  pretty (Application f@(Abstraction _ _) x@(Application _ _))
    = parens (pretty f) <+> parens (pretty x)

  -- Render (λx.x) e instead of λx. x e
  pretty (Application f@(Abstraction _ _) x)
    = parens (pretty f) <+> pretty x

  -- Render f (λx.x) instead of f λ.x x
  pretty (Application f x@(Abstraction _ _))
    = pretty f <+> parens (pretty x)

  -- Render f (g x) instead of f g x
  pretty (Application f x@(Application _ _))
    = pretty f <+> parens (pretty x)

  pretty (Application f x)
    = pretty f <+> pretty x

  pretty (Abstraction x e)
    = text "λ" <> text (unpack (unwords vars))
               <> text "." <+> pretty (snd inner)
    where
      vars  = reverse (fst inner)
      inner = collapse ([x], e)
      collapse (xs, Abstraction x' e') = collapse (x':xs, e')
      collapse (xs, e')                = (xs, e')

  pretty (Variable x)
    = text (unpack x)
