module Blamda.Bound
  ( Var(..)
  , Scope(..)
  , abstract
  , instantiate
  ) where

import Data.Monoid
import Data.Foldable
import Data.Traversable
import Control.Applicative
import Control.Monad.Trans

-- Variables are either bound by an enclosing scope located
-- by values drawn from @b@, or are not, and drawn from @f@
data Var b f
  = Bound b
  | Free  f
  deriving (Eq, Ord, Show)

-- Rename free variables
instance Functor (Var b) where
  fmap f (Free x)  = Free (f x)
  fmap f (Bound x) = Bound x

instance Applicative (Var b) where
  pure                = Free
  Free f  <*> Free x  = Free (f x)
  _       <*> Bound x = Bound x
  Bound f <*> _       = Bound f

-- Instantiate free variables
instance Monad (Var b) where
  return        = Free 
  Free x >>= f  = f x
  Bound x >>= f = Bound x

instance Foldable (Var b) where
  -- :: Monoid m => (f -> m) -> Var b f -> m
  foldMap f (Bound x) = mempty
  foldMap f (Free x)  = f x

instance Traversable (Var b) where
  -- :: Applicative m => (f -> m g) -> Var b f -> m (Var b g)
  traverse f (Bound x) = pure (Bound x)
  traverse f (Free x)  = fmap Free (f x)

newtype Scope b f a
  = Scope { runScope :: f (Var b (f a)) }

instance Functor f => Functor (Scope b f) where
  -- :: (a -> b) -> f (g (h a)) -> f (g (h b))
  fmap f (Scope s) = fmap . fmap . fmap

instance Applicative f => Applicative (Scope b f) where
  -- :: a -> Scope b f a
  pure = Scope . pure . Free . pure

  -- :: Scope b f (a -> d) -> Scope b f a -> Scope b f d
  Scope f <*> Scope x = Scope (f `a3` x)
    where -- op = (<*>) . (<$>) ((<*>) . (<$>) (<*>))
      a3 a b = a2 <$> a <*> b
      a2 a b = a1 <$> a <*> b
      a1 a b = a0 <$> a <*> b
      a0 f x = f x

instance Monad f => Monad (Scope b f) where
  -- :: a -> Scope b f a
  return = Scope . return . Free . return

  -- :: Scope b f a -> (a -> Scope b f g) -> Scope b f g
  Scope x >>= f = undefined

instance Foldable f => Foldable (Scope b f) where
  -- :: Monoid m => (a -> m) -> Scope b f a -> m
  foldMap f (Scope s) = foldMap f s

instance Traversable f => Traversable (Scope b f) where
  -- :: Applicative m => (f -> m g) -> Var b f -> m (Var b g)
  traverse f (Scope s) = traverse f s

instance MonadTrans (Scope b) where
  -- Monad f => f a -> Scope b f a
  lift = return . Free

abstract :: Monad f => (a -> Maybe b) -> f a -> Scope b f a
abstract = undefined

instantiate :: Monad f => (b -> f a)
instantiate = undefined
