-- | A module which provides means of making card pools.
-- While the importing parts of API (MTG.Cards, for instance)
-- use Array as underlying storage, this particular one uses
module MTG.Pools ( boosterNames
                 , booster
                 , singletonNames
                 , singleton
                 , Slot(..) ) where

import Prelude
import Data.List.Random
import Data.List
import Data.Map           as M
import Data.Maybe
import Data.Tuple         as T
import MTG.Cards

type Slot = Card -> Boolean
type CardsL = List (T.Tuple CardId Card)

booster :: forall e. List Slot -> Cards -> (Rnd e) (Maybe Cards)
booster slots cards = boosterDo slots (M.toList cards) (Just Nil)

boosterNames :: forall e. List Slot -> Cards -> (Rnd e) (Maybe (List CardId))
boosterNames slots cards = booster slots cards >>= pure <<< maybe Nothing (\m -> Just $ M.keys m)

singleton :: String
singleton = "???"

singletonNames :: String
singletonNames = "???"

boosterDo :: forall e. List Slot -> CardsL -> Maybe CardsL -> (Rnd e) (Maybe Cards)
boosterDo _                  _  Nothing    = pure Nothing
boosterDo Nil                _  (Just acc) = pure $ Just $ M.fromList acc
boosterDo (Cons slot slots) xs  (Just acc) = do
  let eligible = filter (slot <<< T.snd) xs
  chosen <- choose eligible
  boosterDo slots xs (Just (chosen:acc))
