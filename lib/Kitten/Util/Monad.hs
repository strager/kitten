module Kitten.Util.Monad
  ( concatMapM
  , composeM
  , foldMapM
  , secondM
  , whenJust
  ) where

import Control.Monad
import Data.Monoid
import Data.Traversable (Traversable)

import qualified Data.Foldable as T
import qualified Data.Traversable as T

concatMapM
  :: (Monad m, Traversable t, Monad t)
  => (a -> m (t b)) -> t a -> m (t b)
concatMapM = (liftM join .) . T.mapM

composeM :: (Monad m) => [a -> m a] -> a -> m a
composeM = foldr (>=>) return

foldMapM
  :: (Monoid b, Monad m, Traversable t)
  => (a -> m b) -> t a -> m b
foldMapM = (liftM T.fold .) . T.mapM

secondM :: (Monad m) => (a -> m b) -> (c, a) -> m (c, b)
secondM f (x, y) = do
  y' <- f y
  return (x, y')

whenJust :: (Functor m, Monad m) => Maybe a -> (a -> m b) -> m ()
whenJust (Just x) m = void (m x)
whenJust Nothing _ = return ()
