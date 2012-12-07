{-# LANGUAGE UnicodeSyntax #-}

module Language.Lambad.Misc
  ( lmap
  , swap
  ) where

import Control.Monad.Instances
import Data.Either

swap ∷ Either a b → Either b a
swap = either Right Left

lmap ∷ (a → b) → Either a c → Either b c
lmap f = swap . fmap f . swap
