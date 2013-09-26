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
  fmap g (Pair f s) = Pair (g f) (g s)

instance Applicative Pair where
  pure a = Pair a a
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
  return  = pure
  t >>= f = joinT (fmap f t)

joinT :: Term (Term a) -> Term a
joinT = gfoldT id App Abs distT
  where
    distT :: Nat (Term a) -> Term (Nat a)
    distT Z     = Var Z
    distT (S a) = fmap S a




-- Polymorphic function f :: M a -> N a, where N and M are given
-- type constructors, can be viewed as a collection of functions 
-- (one for each instantiation of the type variable a).
--
-- Because f is polymorphic, these instances are related by the
-- following *naturality condition*
--
--    mapN k . f = f . mapM k




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
foldT v a l = gfoldT v' a l swap . fmap Id
  where
    v' (Id a)       = v a
    swap Z          = Id Z
    swap (S (Id a)) = Id (S a)

-- This is a more general fold operator on the nested datatype Term,
-- with the argument `v` generalized from a to m a for an arbitrary
-- type constructor, and k is used to change Term (Nat (m b)) into
-- Term (m (Nat b)) which fits the recursive call.
gfoldT :: (forall a. m a -> n a)
       -> (forall a. Pair (n a) -> n a)
       -> (forall a. n (Nat a) -> n a)
       -> (forall a. Nat (m a) -> m (Nat a))
       -> Term (m b)
       -> n b
gfoldT v _ _ _ (Var x) = v x
gfoldT v a l k (App p) = a (fmap (gfoldT v a l k) p)
gfoldT v a l k (Abs e) = l (gfoldT v a l k (fmap k e))

-- This term has exactly the same definition as gfoldT, but its type
-- is an instantiation of gfoldT's type (assume isomorphic types are
-- equal) where M = Const a and N = Const b (then M a ~ a and N a ~ b):
kfoldT :: (a -> b)        -- Const a a        -> Const b a
       -> (Pair b -> b)   -- Pair (Const b a) -> Const b a
       -> (b -> b)        -- Const b (Nat a)  -> Const b a
       -> (Nat a -> a)    -- Nat (Const a a)  -> Const a (Nat a)
       -> Term a          -- Term (Const a b)
       -> b               -- Const b b
kfoldT v _ _ _ (Var x) = v x
kfoldT v a l k (App p) = a (fmap (kfoldT v a l k) p)
kfoldT v a l k (Abs e) = l (kfoldT v a l k (fmap k e))

showT :: Term String -> String
showT = kfoldT id showP ('L':) showI
  where
    showP (Pair f s)
                = "(" ++ f ++ " " ++ s ++ ")"
    showI Z     = "Z"
    showI (S a) = 'S':a

showC :: Term Char -> String
showC = showT . fmap return




-- Creating a new abstraction over a free variable x in E means each
-- free occurrence of x becomes Z (zero) and every other variable is
-- incremented: y becomes S y.
abstract :: Eq a => a -> Term a -> Term a
abstract x = Abs . fmap (match x)

match :: Eq a => a -> a -> Nat a
match n m
  | n == m    = Z
  | otherwise = S m

-- Function application
apply :: Term a -> Term (Nat a) -> Term a
apply t = joinT . fmap (subst t . fmap Var)

-- This is the left-inverse of match t
subst :: a -> Nat a -> a
subst _ (S x) = x
subst x _     = x

