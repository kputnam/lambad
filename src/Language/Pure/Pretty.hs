{-# LANGUAGE UnicodeSyntax #-}

module Language.Pure.Pretty
  ( renderText
  , renderString
  , Pretty (..)
  ) where

import Prelude   hiding (unwords)
import Data.Text hiding (reverse)
import Text.PrettyPrint

import Language.Pure.Syntax

renderText ∷ Pretty a => a → Text
renderText = pack . render . pretty

renderString ∷ Pretty a => a → String
renderString = render . pretty

class Pretty a where
  pretty ∷ a → Doc

instance Pretty Declaration where
  pretty (Declaration x e)
    = parens $ text "define" <+> text (unpack x) <+> pretty e

instance Pretty Expression where
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
    = text "λ" <> text (unpack (unwords xs))
               <> text "." <+> pretty e'
    where
      xs    = reverse (fst inner)
      e'    = snd inner
      inner = collapse ([x], e)
      collapse (xs, Abstraction x e) = collapse (x:xs, e)
      collapse (xs, e)               = (xs, e)

  pretty (Variable x)
    = text (unpack x)
