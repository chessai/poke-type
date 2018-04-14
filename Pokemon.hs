{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RoleAnnotations   #-}
{-# LANGUAGE TypeFamilies      #-}

module Pokemon where

import Data.ByteString (ByteString)
import Data.Kind (Constraint)

import Prelude hiding (Either)

data PType = None | Fire | Water | Grass

type family OneOf (x :: PType) (a :: PType) (b :: PType) :: Constraint where
  OneOf x x _ = ()
  OneOf x _ x = () 
  OneOf x y z = () ~ Int

type family Different a b :: Constraint where
  Different a a = () ~ Int
  Different a b = ()

data Pokemon (a :: PType) (b :: PType) where
  Poke
    :: forall a b x. (Different a b, OneOf x a b)
    => Move x
    -> ByteString -- name
    -> Int -- health
    -> Pokemon a b
type role Pokemon nominal nominal

data Move (a :: PType) where
  MkMove :: Int -> Move a
type role Move phantom

ember :: Move Fire
ember = MkMove 5

vineWhip :: Move Grass
vineWhip = MkMove 5

bubble :: Move Water
bubble = MkMove 5

charmander :: Pokemon Fire None
charmander = Poke ember "Charmander" 20

bulbasaur :: Pokemon Grass None
bulbasaur = Poke vineWhip "Bulbasaur" 20

squirtle :: Pokemon Water None
squirtle = Poke bubble "Squirtle" 20

attack
  :: Pokemon a b -- ^ attacker
  -> Pokemon x y -- ^ receiver of attack
  -> Pokemon x y -- ^ result of being attacked
attack (Poke (MkMove i) _ h) (Poke m' n' h') = Poke m' n' (h' - i)

battle :: Pokemon a b -> Pokemon x y -> ByteString
battle (Poke _ _ 0) (Poke _ n' _) = n'
battle (Poke _ n _) (Poke _ _  0) = n
battle p p' = battle (attack p p') p


