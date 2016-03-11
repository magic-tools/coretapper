-- | A module which provides means of making card pools.
-- While the importing parts of API (MTG.Cards, for instance)
-- use Array as underlying storage, this particular one uses
module MTG.Pools ( boosterNames
                 , booster
                 , singletonNames
                 , singletonNamesNames
                 , singleton
                 , Slot(..) ) where

import Prelude
import Data.List.Random
import Data.List
import Data.Map           as M
import Data.Maybe
import Data.Tuple         as T
import MTG.Cards

-- | Function type which takes a card candidate as the second argument, and
-- a map of already generated cards in this pass, and returns True
-- if the given card is eligible of joining the pool.
type Slot = Cards -> Card -> Boolean

-- | A list equivalent of Cards type.
type CardsL = List (T.Tuple CardId Card)

-- | Generates a booster from a pool of cards, based on List of Slots. Cards in booster may repeat.
booster :: forall e. List Slot -> Cards -> (Rnd e) (Maybe Cards)
booster slots cards = boosterDo slots (M.toList cards) (Just Nil)

boosterNames :: forall e. List Slot -> Cards -> (Rnd e) (Maybe (List CardId))
boosterNames slots cards = booster slots cards >>= pure <<< maybe Nothing (\m -> Just $ M.keys m)

-- | Generates a pool with singlegon cards based on List of Slots. Cards which are picked are removed
-- from the initial pool. The new version of the pool is returned as the second element of the tuple.
singleton :: forall e. List Slot -> Cards -> (Rnd e) (Maybe (T.Tuple Cards Cards))
singleton slots cards = singletonDo slots (M.toList cards) (Just Nil)

singletonNames :: forall e. List Slot -> Cards -> (Rnd e) (Maybe (T.Tuple (List CardId) Cards))
singletonNames slots cards = singleton slots cards >>= pure <<< maybe Nothing (\(T.Tuple x y) -> Just (T.Tuple (M.keys x) y))

singletonNamesNames :: forall e. List Slot -> Cards -> (Rnd e) (Maybe (T.Tuple (List CardId) (List CardId)))
singletonNamesNames slots cards = singleton slots cards >>= pure <<< maybe Nothing 
                                                                           (\(T.Tuple x y) -> Just (T.Tuple (M.keys x) (M.keys y)))

boosterDo :: forall e. List Slot -> CardsL -> Maybe CardsL -> (Rnd e) (Maybe Cards)
boosterDo _                  _   Nothing    = pure Nothing
boosterDo Nil                _   (Just acc) = pure $ Just $ M.fromList acc
boosterDo (Cons slot slots)  xs  (Just acc) = do
  let eligible = filter (slot (M.fromList acc) <<< T.snd) xs
  chosen <- choose eligible
  boosterDo slots xs (Just (chosen:acc))

singletonDo :: forall e. List Slot -> CardsL -> Maybe CardsL -> (Rnd e) (Maybe (T.Tuple Cards Cards))
singletonDo _                 _  Nothing   = pure Nothing
singletonDo Nil               xs (Just ys) = pure $ Just $ T.Tuple (M.fromList ys) (M.fromList xs)
singletonDo (Cons slot slots) xs (Just ys) = do
  let eligible = filter (slot (M.fromList ys) <<< T.snd) xs
  chosen <- choose eligible
  let xs1 = filter ((/=) (T.fst chosen) <<< T.fst) xs
  singletonDo slots xs1 (Just (chosen:ys))
