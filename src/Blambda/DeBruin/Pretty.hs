module Blambda.DeBruijn.Syntax
  ( Term(..)
  , Declaration(..)
  ) where

import Lambad.Pretty

instance Pretty Declaration where
  pretty (Declaration x e)
    = parens $ text "define" <+> text (unpack x) <+> pretty e

instance Pretty Term where
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
