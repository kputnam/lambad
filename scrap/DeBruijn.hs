{-# LANGUAGE RankNTypes #-}

-- de Brujin notation as a nested datatype
--    R. S. Bird, R. Paterson (1999)

module DeBruijn where
import Control.Applicative

newtype Id a
  = Id { runId :: a }




data Pair a
  = Pair a a
  deriving (Show, Read, Eq, Ord)

instance Functor Pair where
  fmap f (Pair fst snd) = Pair (f fst) (f snd)

instance Applicative Pair where
  pure a                = Pair a a
  Pair f g <*> Pair x y = Pair (f x) (g y)




data Nat a
  = Z
  | S a
  deriving (Show, Read, Eq, Ord)

instance Functor Nat where
  fmap _ Z      = Z
  fmap f (S a)  = S (f a)

instance Applicative Nat where
  pure        = S
  Z <*> _     = Z
  _ <*> Z     = Z
  S f <*> S a = S (f a)

instance Monad Nat where
  return    = pure
  Z   >>= f = Z
  S a >>= f = f a




data Term a
  = Var a
  | App (Pair (Term a))
  | Abs (Term (Nat a))
  deriving (Show, Read, Eq, Ord)

instance Functor Term where
  fmap f (Var a) = Var (f a)
  fmap f (App p) = App (fmap (fmap f) p)
  fmap f (Abs e) = Abs (fmap (fmap f) e)

instance Applicative Term where
  pure            = Var
  Var f <*> Var a = Var (f a)
  f     <*> App p = error "todo"
  f     <*> Abs p = error "todo"

instance Monad Term where
  return      = pure
  Var a >>= f = f a
  App p >>= f = error "todo"
  Abs e >>= f = error "todo"


-- Polymorphic function f :: M a -> N a, where N and M are given
-- type constructors, can be viewed as a collection of functions 
-- (one for each instantiation of the type variable a).
--
-- Because f is polymorphic, these instances are related by the
-- following *naturality condition*
--
--    mapN k . f = f . mapM k
--
--
--
-- N is an arbitrary type constructor. Since v, a, and l are
-- themselves natural transformations, `foldT v a l` is also
-- a natural transformation:
--
--   mapN k . foldT v a l = foldT v a l . mapT k
--
-- The naturality law implies no instance of foldT can manipulate
-- the values of free variables (represented as type variable b).
--
foldT :: (forall a. a -> n a)          -- Id a       -> N a
      -> (forall a. Pair (n a) -> n a) -- Pair (N a) -> Id (N a)
      -> (forall a. n (Nat a) -> n a)  -- N (Nat a)  -> N (Id a)
      -> Term b
      -> n b
-- foldT v _ _ (Var x) = v x
-- foldT v a l (App p) = a (fmap (foldT v a l) p)
-- foldT v a l (Abs e) = l (foldT v a l e)
foldT v a l = goldT v' a l swap . fmap Id
  where
    v' (Id a)       = v a
    swap Z          = Id Z
    swap (S (Id a)) = Id (S a)


-- This is a more general fold operator on the nested datatype Term,
-- with the argument `v` generalized from a to m a for an arbitrary
-- type constructor, and k is used to change Term (Nat (m b)) into
-- Term (m (Nat b)) which fits the recursive call.
goldT :: (forall a. m a -> n a)
      -> (forall a. Pair (n a) -> n a)
      -> (forall a. n (Nat a) -> n a)
      -> (forall a. Nat (m a) -> m (Nat a))
      -> Term (m b)
      -> n b
goldT v _ _ _ (Var x) = v x
goldT v a l k (App p) = a (fmap (goldT v a l k) p)
goldT v a l k (Abs e) = l (goldT v a l k (fmap k e))
