module Kitten.Util.Set
  ( mapM
  ) where

import Data.Set (Set)
import Prelude hiding (mapM)

import qualified Control.Monad
import qualified Data.Set as Set

mapM
  :: (Monad m, Ord a, Ord b)
  => (a -> m b)
  -> Set a
  -> m (Set b)
mapM f
  = Control.Monad.liftM Set.fromList
  . Control.Monad.mapM f
  . Set.toList
