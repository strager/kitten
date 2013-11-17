{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Kitten.NameMap
  ( NameMap
  , (!)
  , empty
  , fromList
  , insert
  , insertWith
  , lookup
  , map
  , member
  , singleton
  , toList
  ) where

import Control.Arrow (first)
import Data.IntMap (IntMap)
import Data.Monoid (Monoid)
import Prelude hiding (lookup, map)

import qualified Data.IntMap as I
import qualified Prelude

import Kitten.Name

newtype NameMap a = NameMap (IntMap a)
  deriving (Eq, Functor, Monoid)

(!) :: NameMap a -> Name -> a
NameMap names ! Name index = names I.! index

empty :: NameMap a
empty = NameMap I.empty

fromList :: [(Name, a)] -> NameMap a
fromList = NameMap . foldr
  (\(Name index, value) acc -> I.insert index value acc)
  I.empty

insert :: Name -> a -> NameMap a -> NameMap a
insert (Name index) value (NameMap names)
  = NameMap $ I.insert index value names

insertWith :: (a -> a -> a) -> Name -> a -> NameMap a -> NameMap a
insertWith f (Name index) value (NameMap names)
  = NameMap $ I.insertWith f index value names

map :: (a -> b) -> NameMap a -> NameMap b
map = fmap

lookup :: Name -> NameMap a -> Maybe a
lookup (Name index) (NameMap names)
  = I.lookup index names

member :: Name -> NameMap a -> Bool
member (Name index) (NameMap names)
  = I.member index names

singleton :: Name -> a -> NameMap a
singleton (Name index) value
  = NameMap $ I.singleton index value

toList :: NameMap a -> [(Name, a)]
toList (NameMap names)
  = Prelude.map (first Name) $ I.toList names
