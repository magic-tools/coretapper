-- | A module providing ways to work with Magic cards.
-- For now, we naively say that everything is a String, later on
-- we'll make a separate library which will provide very smart
-- semantical types for Magic cards.
module MTG.Cards ( mkCards
                 , mkSets
                 , Card(..)
                 , CardId(..)
                 , CardType(..)
                 , Cards(..)
                 , Color(..)
                 , CompRarity(..)
                 , Cost(..)
                 , Loyalty(..)
                 , Power(..)
                 , Rarity(..)
                 , Set(..)
                 , SetId(..)
                 , Sets(..)
                 , Subtype(..)
                 , Toughness(..) ) where

import Prelude (class Ord, class Eq, class Show, return, ($), (<$>), bind)
import Data.Generic (class Generic, gCompare, gEq, gShow)

import Data.Array                      as A
import Data.Either                     (Either(..))
import Data.Foreign                    as F
import Data.Foreign.Class              as FC
import Data.Foreign.NullOrUndefined    (runNullOrUndefined)
import Data.Map                        as M
import Data.Maybe                      (Maybe(..))
import Data.Tuple                      as T

map :: forall a b. (a -> b) -> Array a -> Array b
map f = A.mapMaybe (\x -> Just $ f x)

mkCards :: String -> F.F Cards
mkCards j = g (FC.readJSON j :: F.F (Array Card))
  where
    g :: F.F (Array Card) -> F.F Cards
    g (Left xs)  = Left xs
    g (Right xs) = Right $ M.fromFoldable $ map (\(Card x) -> T.Tuple x.name (Card x)) xs

mkSets :: String -> F.F Sets
mkSets j = g (FC.readJSON j :: F.F (Array Set))
  where
    g :: F.F (Array Set) -> F.F Sets
    g (Left xs)  = Left xs
    g (Right xs) = Right $ M.fromFoldable $ map (\(Set x) -> T.Tuple x.code (Set x)) xs

type Cards = M.Map CardId Card
type Sets  = M.Map SetId  Set

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
type CompRarity  = Either (Array String) String
type Rarity      = String
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
                 , rarity           :: Maybe Rarity
                 , oracle           :: Maybe String
                 , manaCost         :: Maybe Cost
                 , cmc              :: Maybe Number
                 , types            :: Maybe (Array CardType)
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

data Set = Set { cards    :: Array CardId
               , code     :: SetId
               , name     :: Maybe String }
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
    rarity        <- runNullOrUndefined <$> FC.readProp "rarity" j
    oracle        <- runNullOrUndefined <$> FC.readProp "oracle" j
    manaCost      <- runNullOrUndefined <$> FC.readProp "manaCost" j
    cmc           <- runNullOrUndefined <$> FC.readProp "cmc" j
    types         <- runNullOrUndefined <$> FC.readProp "types" j
    subtypes      <- runNullOrUndefined <$> FC.readProp "subtypes" j
    colors        <- runNullOrUndefined <$> FC.readProp "colors" j
    colorRelation <- runNullOrUndefined <$> FC.readProp "colorRelation" j
    power         <- runNullOrUndefined <$> FC.readProp "power" j
    toughness     <- runNullOrUndefined <$> FC.readProp "toughness" j
    loyalty       <- runNullOrUndefined <$> FC.readProp "loyalty" j
    return $ Card { name:           name
                  , rarity:         rarity
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

instance isForeignSet :: FC.IsForeign Set where
  read j = do
    cards      <- FC.readProp "cards" j
    code       <- FC.readProp "code" j
    name       <- runNullOrUndefined <$> FC.readProp "name" j
    return $ Set { cards: cards
                 , code:  code
                 , name:  name }
