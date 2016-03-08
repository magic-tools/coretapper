-- | A module providing ways to work with Magic cards.
-- For now, we naively say that everything is a String, later on
-- we'll make a separate library which will provide very smart
-- semantical types for Magic cards.
module MTG.Cards ( Sets(..)
                 , Cards(..)
                 , Card(..)
                 , CardType(..)
                 , Color(..)
                 , Cost(..)
                 , Id(..)
                 , Power(..)
                 , Rarity(..)
                 , Set(..)
                 , Subtype(..)
                 , Toughness(..) ) where

import Data.Either    (Either())
import Data.Map       as M
import Data.Maybe     (Maybe())

newtype Sets   = Sets  { getSets  :: M.Map (Id Set)  Set  }
newtype Cards  = Cards { getCards :: M.Map (Id Card) Card }

newtype Id a = Id { getId :: String }

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

data Card = Card { name             :: Id Card
                 , rarity           :: Rarity
                 , oracle           :: String
                 , manaCost         :: Cost
                 , cmc              :: Int
                 , types            :: Array CardType
                 , subtypes         :: Maybe (Array Subtype)
                 , colors           :: Maybe (Array Color)
                 , colorRelation    :: Maybe (Array Color)
                 , power            :: Maybe Power
                 , toughness        :: Maybe Toughness }

data Set = Set { cards    :: Array Card
               , booster  :: Maybe (Array Rarity)
               , code     :: Id Set
               , name     :: String }
