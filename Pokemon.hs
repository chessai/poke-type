{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Pokemon where

import Data.Type.Equality
import           Prelude (Bool(..))
import qualified Prelude

data PType = None | Fire | Water | Grass
  deriving (Prelude.Eq)

type family (a :: k) /= (b :: k) :: Bool
infix 4 /=

type family NeqPType (a :: PType) (b :: PType) :: Bool where
  NeqPType None None   = False
  NeqPType None _      = True
  NeqPType Fire Fire   = False
  NeqPType Fire _      = True
  NeqPType Water Water = False
  NeqPType Water _     = True
  NeqPType Grass Grass = False
  NeqPType Grass _     = True

type instance a /= b = NeqPType a b

-- | this should succeed to compile
--foo :: (Fire /= None) ~ True => ((Fire /= None) :~: True)
--foo = Refl

-- | this should fail to compile
--bar :: (Fire /= Fire) ~ True => ((Fire /= Fire) :~: True)
--bar = Refl

type PNeq a b = (a /= b) :~: True

data Pokemon a b where
  Poke :: (a ~ PType, b ~ PType, (a /= b) ~ True) => a -> b -> Pokemon a b

charmander :: (a ~ PType, b ~ PType, (a /= b) ~ True) => Pokemon a b
charmander = Poke Fire None

badBulbasaur :: (a ~ PType, b ~ PType, (a /= b) ~ True) => Pokemon a b
badBulbasaur = Poke Grass Grass

data Nat = Z | S Nat

infixl 6 +,-
infixl 7 *

type family (n :: Nat) + (m :: Nat) :: Nat where
  Z + m = m
  (S n) + m = S (n + m)

type family (n :: Nat) * (m :: Nat) :: Nat where
  Z * m = Z
  (S n) * m = (n * m) + m

type family (n :: Nat) - (m :: Nat) :: Nat where
  Z - m = Z
  (S n) - Z = S n
  (S n) - m = S (n - m)

type family Min (n :: Nat) (m :: Nat) :: Nat where
  Min Z Z = Z
  Min Z (S m) = Z 
  Min (S n) Z = Z
  Min (S n) (S m) = S (Min n m)

type family Max (n :: Nat) (m :: Nat) :: Nat where
  Max Z Z = Z
  Max Z (S m) = S m 
  Max (S n) Z = S n
  Max (S n) (S m) = S (Max n m)

data Vec a (n :: Nat) where
  Nil :: Vec a Z
  Cons :: a -> Vec a n -> Vec a (S n)

deriving instance Prelude.Eq a => Prelude.Eq (Vec a n)

type family Length (xs :: Vec a (n :: Nat)) :: Nat where
  Length Nil = Z
  Length (Cons x xs) = (S Z) + Length xs

type family Contains (xs :: Vec a (n :: Nat)) (elem :: a) :: Bool where
  Contains Nil _ = False
  Contains (Cons x xs) x = True
  Contains (Cons x xs) y = Contains xs y

data Set a where
  Set :: Vec a n -> Set a

infixr 5 ++
(++) :: Vec a n -> Vec a m -> Vec a (n + m)
(Cons x xs) ++ ys = Cons x (xs ++ ys)
Nil         ++ ys = ys

type family Head (xs :: Vec a (n :: Nat)) :: a where
  Head (Cons x _) = x

head :: Vec a (S n) -> a
head (Cons x _) = x

type family Last (xs :: Vec a (n :: Nat)) :: a where
  Last (Cons x Nil) = x
  Last (Cons x xs)  = Last xs

last :: Vec a n -> a
last (Cons x Nil) = x
last (Cons x xs)  = last xs

type family Tail (xs :: Vec a ((S n) :: Nat)) :: Vec a (n :: Nat) where
  Tail (Cons _ xs) = xs

tail :: Vec a (S n) -> Vec a n
tail (Cons _ xs) = xs

type family Init (xs :: Vec a ((S n) :: Nat)) :: Vec a (n :: Nat) where
  Init (Cons _ Nil) = Nil
  Init (Cons x xs ) = Cons x (Init xs)

init :: Vec a (S n) -> Vec a n
init (Cons x xs) =
  case xs of
    Nil -> Nil
    (Cons _ _) -> Cons x (init xs)

--type family Uncons (xs :: Vec a ((S n) :: Nat)) :: (a, Vec a (n :: Nat)) where
 -- Uncons (Cons x xs) = (x, xs)

uncons :: Vec a (S n) -> (a, Vec a n)
uncons (Cons x xs) = (x, xs)

null :: Vec a n -> Bool
null Nil = True
null _   = False

vmap :: (a -> b) -> Vec a n -> Vec b n
vmap _ Nil = Nil
vmap f (Cons x xs) = Cons (f x) (vmap f xs)

foldl :: (a -> b -> a) -> a -> Vec b n -> a
foldl _ x Nil = x
foldl f y (Cons x xs) = foldl f (f y x) xs

foldr :: (a -> b -> b) -> b -> Vec a n -> b
foldr _ y Nil = y
foldr f y (Cons x xs) = f x (foldr f y xs)

toList :: Vec a n -> [a]
toList Nil = []
toList (Cons x xs) = x : toList xs

zipWithSame :: (a -> b -> c) -> Vec a n -> Vec b n -> Vec c n
zipWithSame _ Nil Nil = Nil
zipWithSame f (Cons a as) (Cons b bs) = Cons (f a b) (zipWithSame f as bs)

zipWith :: (a -> b -> c) -> Vec a n -> Vec b m -> Vec c (Min n m)
zipWith _ Nil Nil = Nil
zipWith _ Nil (Cons _ _) = Nil
zipWith _ (Cons _ _) Nil = Nil
zipWith f (Cons a as) (Cons b bs) = Cons (f a b) (zipWith f as bs)

instance Prelude.Functor Set where
  fmap _ (Set Nil) = Set Nil
  fmap f (Set (Cons x xs)) = Set Prelude.$ Cons (f x) (vmap f xs)

