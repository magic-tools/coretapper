module Main where

import Control.Monad.Eff            (Eff)
import Control.Monad.Eff.Console    (CONSOLE, log)
import Control.Monad.Eff.Exception  (EXCEPTION())
import Data.Argonaut.Parser         (jsonParser)
import Data.Either                  (Either(..))
import Data.Maybe                   (Maybe())
import Node.Encoding                (Encoding(UTF8))
import Node.FS                      (FS())
import Node.FS.Sync                 (readTextFile)
import Partial.Unsafe               (unsafePartial, unsafeCrashWith)
import Prelude

import Data.Map as M

newtype AllSets  = AllSets  { getAllSets  :: M.Map SetId Set }
newtype AllCards = AllCards { getAllCards :: M.Map CardId Card }

type SetId   = String
data Set     = Set { cards   :: M.Map CardId Card
                   , booster :: M.Map Rarity Int
                   , code    :: SetId
                   , name    :: String }

type CardId   = String
type Rarity   = String
type CardType = String
data Color    = W
              | U
              | B
              | R
              | G

data Card    = Card { name     :: CardId
                    , rarity   :: Rarity
                    , oracle   :: String
                    , manaCost :: String
                    , types    :: Array CardType
                    , colors   :: Array Color
                    , pt       :: Maybe String
                    , sets     :: Array SetId }

type MainEff a = Eff ( console :: CONSOLE
                     , fs      :: FS
                     , err     :: EXCEPTION | a )

main :: forall e. (MainEff e) Unit
main = do
  x <- readTextFile UTF8 "priv/WIP.json"
  let y = (unsafePartial g <<< jsonParser) x
  (log <<< show) y
  where
    g :: forall a b. (Partial) => Either a b -> b
    g (Right x) = x
    g _         = unsafeCrashWith "Invalid JSON in priv/"
