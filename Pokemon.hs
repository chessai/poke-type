{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RoleAnnotations   #-}
{-# LANGUAGE TypeFamilies      #-}

module Pokemon where

import Data.ByteString (ByteString)
import Data.Kind (Constraint)
import Data.String (IsString)

data PType = None | Normal | Fire | Water | Grass

type family OneOf (x :: PType) (a :: PType) (b :: PType) :: Constraint where
  OneOf Normal _ _ = () 
  OneOf x      x _ = ()
  OneOf x      _ x = () 
  OneOf x      y z = () ~ Int

type family Different a b :: Constraint where
  Different a a = () ~ Int
  Different a b = ()

type Complies u v w y a b = (OneOf u a b, OneOf v a b, OneOf w a b, OneOf y a b)

newtype HP = HP Int deriving (Num)
newtype Name = Name ByteString deriving (IsString)

data Pokemon (a :: PType) (b :: PType) where
  Poke
    :: forall a b u v w y. (Different a b, Complies u v w y a b)
    => MoveSet u v w y
    -> Name
    -> HP
    -> Pokemon a b
type role Pokemon nominal nominal

data MoveSet a b c d = MoveSet (Move a) (Move b) (Move c) (Move d)

fromTuple :: (Move a,Move b,Move c,Move d) -> MoveSet a b c d
fromTuple (a,b,c,d) = MoveSet a b c d
{-# INLINE fromTuple #-}

newtype Move (a :: PType) where
  MkMove :: Int -> Move a
type role Move phantom

empty :: Move Normal
empty = MkMove 0

tackle :: Move Normal
tackle = MkMove 3

scratch :: Move Normal
scratch = MkMove 3

ember :: Move Fire
ember = MkMove 5

vineWhip :: Move Grass
vineWhip = MkMove 5

bubble :: Move Water
bubble = MkMove 5

charmander :: Pokemon Fire None
charmander = Poke mvset nm hp
  where
    mvset = fromTuple (ember, scratch, empty, empty)
    nm = "Charmander"
    hp = 20

bulbasaur :: Pokemon Grass None
bulbasaur = Poke mvset nm hp
  where
    mvset = fromTuple (vineWhip, tackle, empty, empty)
    nm = "Bulbasaur"
    hp = 20

squirtle :: Pokemon Water None
squirtle = Poke mvset nm hp
  where
    mvset = fromTuple (bubble, tackle, empty, empty)
    nm = "Squirtle"
    hp = 20

--attack
--  :: Pokemon a b -- ^ attacker
--  -> Pokemon x y -- ^ receiver of attack
--  -> Pokemon x y -- ^ result of being attacked
--attack (Poke (MkMove i) _ h) (Poke m' n' h') = Poke m' n' (h' - i)

--battle :: Pokemon a b -> Pokemon x y -> ByteString
--battle (Poke _ _ 0) (Poke _ n' _) = n'
--battle (Poke _ n _) (Poke _ _  0) = n
--battle p p' = battle (attack p p') p
