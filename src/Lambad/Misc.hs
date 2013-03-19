module Lambad.Misc
  ( lmap
  , swap
  , applyM2
  ) where

import Control.Monad
import Control.Monad.Instances ()

swap :: Either a b -> Either b a
swap = either Right Left

lmap :: (a -> b) -> Either a c -> Either b c
lmap f = swap . fmap f . swap

applyM2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
applyM2 f a b = join $ liftM2 f a b
