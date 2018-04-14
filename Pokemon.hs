{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE EmptyCase     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeInType    #-}

module Pokemon where

import Data.Kind (Constraint)
import Type.Reflection ((:~:)(..))

data PType = None | Fire | Water | Grass

-- If 'a' and 'b' are different - the constraint
-- cannot be solved as the construction '() ~ Int'
-- is pathological.
type family Different a b :: Constraint where
  Different a a = () ~ Int
  Different a b = ()

data Pokemon (a :: PType) (b :: PType) where
  Poke :: Different a b => Pokemon a b

charmander :: Pokemon Fire None
charmander = Poke

-- this will fail to compile
-- bulbasaur :: Pokemon Grass Grass
-- bulbasaur = Poke
