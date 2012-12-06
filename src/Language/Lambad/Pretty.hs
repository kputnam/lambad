module Language.Lambad.Pretty
  ( renderText
  , renderString
  , Pretty (..)
  ) where

import Language.Lambad.Syntax
import Data.Text
import Text.PrettyPrint

renderText :: Pretty a => a -> Text
renderText = pack . render . pretty

renderString :: Pretty a => a -> String
renderString = render . pretty

class Pretty a where
  pretty :: a -> Doc

instance Pretty Declaration where
  pretty (Declaration x e)
    = parens $ text "define" <+> text (unpack x) <+> pretty e

instance Pretty Expression where
  pretty (Application f x)
    = parens $ pretty f <+> pretty x

  pretty (Abstraction x e)
    = parens $ text "lambda" <+> text (unpack x) <> text "." <+> pretty e

  pretty (Variable x)
    = text (unpack x)
