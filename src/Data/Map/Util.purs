-- | A module exporting some of the missing utils from Data.Map.
-- It doesn't use underlying 2-3 tree, instead it is using primitives
-- provided by Data.Map and conversions to List, hence, being ridiculously
-- slow.
--
-- TODO: Implement those as functions that work on underlying data
-- type directly.
module Data.Map.Util ( filter
                     , filterWithKey ) where

import Prelude
import Data.Map
import Data.List as L
import Data.Tuple as T

filter :: forall k v. (Ord k) => (v -> Boolean) -> Map k v -> Map k v
filter f = filterWithKey (\_ x -> f x)

filterWithKey :: forall k v. (Ord k) => (k -> v -> Boolean) -> Map k v -> Map k v
filterWithKey f m = filterWithKey_ f (toList m)

filterWithKey_ :: forall k v. (Ord k) => (k -> v -> Boolean) -> L.List (T.Tuple k v) -> Map k v
filterWithKey_ f xs = fromList $ L.filter (\(T.Tuple k v) -> f k v) xs
