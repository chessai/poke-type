{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}

module Pokemon where

import Data.Type.Equality

data PType = None | Fire | Water | Grass
  deriving (Eq)

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

data Pokemon (a :: PType) (b :: PType) where
  Poke :: (PNeq a b) -> Pokemon a b

type PNeq (a :: PType) (b :: PType) = (a /= b) :~: True

charmander :: Pokemon Fire None
charmander = Poke Refl

-- this will fail to compile
--bulbasaur :: Pokemon Grass Grass
--bulbasaur = Poke Refl 
