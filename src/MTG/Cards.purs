-- | A module providing ways to work with Magic cards.
-- For now, we naively say that everything is a String, later on
-- we'll make a separate library which will provide very smart
-- semantical types for Magic cards.
module MTG.Cards ( Card(..)
                 , CardId(..)
                 , CardType(..)
                 , Cards(..)
                 , Color(..)
                 , Cost(..)
                 , Loyalty(..)
                 , Power(..)
                 , Rarity(..)
                 , Set(..)
                 , SetId(..)
                 , Sets(..)
                 , Subtype(..)
                 , Toughness(..) ) where

import Prelude
import Data.Generic (class Generic, gCompare, gEq, gShow)

import Data.Either                     (Either(..))
import Data.Foreign                    as F
import Data.Foreign.Class              as FC
import Data.Foreign.NullOrUndefined    (runNullOrUndefined)
import Data.Map                        as M
import Data.Maybe                      (Maybe())

newtype Sets   = Sets (M.Map SetId Set)

newtype Cards  = Cards (M.Map CardId Card)

{--
-- | I still have no idea regarding the way newtypes work
-- in purescript, so we're not going to use phantom newtype
-- hack to increase type safety :(
newtype Id a = Id { getId :: String }
--}

-- For now, everything is a String. I know, it sucks, but we're
-- yet to figure out the semantic requirements of the types and
-- we're yet to figure out a strategy to send values of types from
-- the backend to the browser

-- | Sometimes Rarity is [ "mythic rare", "rare" ], hence `Left (Array String)` option
type Rarity      = Either (Array String) String
type CardType    = String
type Color       = String
type Cost        = String
type Power       = String
type Subtype     = String
type Toughness   = String
type Loyalty     = String
type CardId      = String
type SetId       = String

data Card = Card { name             :: CardId
                 , rarity           :: Rarity
                 , oracle           :: String
                 , manaCost         :: Cost
                 , cmc              :: Int
                 , types            :: Array CardType
                 , subtypes         :: Maybe (Array Subtype)
                 , colors           :: Maybe (Array Color)
                 , colorRelation    :: Maybe (Array Color)
                 , power            :: Maybe Power
                 , toughness        :: Maybe Toughness
                 , loyalty          :: Maybe Loyalty }
derive instance genericCard :: Generic Card
instance showCard :: Show Card where
  show = gShow
instance eqCard :: Eq Card where
  eq = gEq
instance ordCard :: Ord Card where
  compare = gCompare

data Set = Set { cards    :: Array Card
               , booster  :: Maybe (Array Rarity)
               , code     :: SetId
               , name     :: String }
derive instance genericSet :: Generic Set
instance showSet :: Show Set where
  show = gShow
instance eqSet :: Eq Set where
  eq = gEq
instance ordSet :: Ord Set where
  compare = gCompare

{--
instance isForeignCards :: FC.IsForeign Cards where
  read j = readCardAcc j M.empty

readCardAcc []     m = return m
readCardAcc (c:cs) m = M.insert 
--}

-- | TODO: consider http://www.purescript.org/learn/generic/
instance isForeignCard :: FC.IsForeign Card where
  read j = do
    name          <- FC.readProp "name" j
    rarity        <- FC.readProp "rarity" j
    oracle        <- FC.readProp "oracle" j
    manaCost      <- FC.readProp "manaCost" j
    cmc           <- FC.readProp "cmc" j
    types         <- FC.readProp "types" j
    subtypes      <- optProp  "subtypes" j
    colors        <- optProp  "colors" j
    colorRelation <- optProp  "colorRelation" j
    power         <- optProp  "power" j
    toughness     <- optProp  "toughness" j
    loyalty       <- optProp  "loyalty" j
    return $ Card { name:           name
                  , rarity:         Right rarity
                  , oracle:         oracle
                  , manaCost:       manaCost
                  , cmc:            cmc
                  , types:          types
                  , subtypes:       subtypes
                  , colors:         colors
                  , colorRelation:  colorRelation
                  , power:          power
                  , toughness:      toughness
                  , loyalty:        loyalty }
    where
      optProp x y = runNullOrUndefined <$> FC.readProp x y
