{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE EmptyCase     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wall -Werror #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Pokemon where

import Data.Void
import Type.Reflection ((:~:)(..))

data PType = None | Fire | Water | Grass

data Pokemon (a :: PType) (b :: PType) where
  Poke :: (a :~: b -> Void) -> Pokemon a b

charmander :: Pokemon Fire None
charmander = Poke (\case {})

-- this will fail to compile
--
--
-- Poke1.hs: [-Wincomplete-patterns]
--     Pattern match(es) are non-exhaustive
--     In a case alternative: Patterns not matched: Refl
--   |
--   | bulbasaur = Poke (\case {})
--   |        
-- bulbasaur :: Pokemon Grass Grass
-- bulbasaur = Poke (\case {})
