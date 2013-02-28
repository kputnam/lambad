module Lambad.Pretty
  ( renderText
  , renderString
  , Pretty (..)
  ) where

import Data.Text hiding (reverse)
import Text.PrettyPrint

renderText :: Pretty a => a -> Text
renderText = pack . render . pretty

renderString :: Pretty a => a -> String
renderString = render . pretty

class Pretty a where
  pretty :: a -> Doc
