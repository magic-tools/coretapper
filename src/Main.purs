module Main where

import Control.Monad.Eff            (Eff)
import Control.Monad.Eff.Console    (CONSOLE(), log)
import Control.Monad.Eff.Exception  (EXCEPTION())
import Control.Monad.Eff.Random     (RANDOM())
import Data.Either                  (Either(..))
--import Data.Maybe                   (Maybe())
import Node.Encoding                (Encoding(UTF8))
import Node.FS                      (FS())
import Node.FS.Sync                 (readTextFile)
import Partial.Unsafe               (unsafePartial, unsafeCrashWith)
import Prelude                      (Unit, ($), show, bind, (==))

import Data.List     as L
import Data.Map      as M
import Data.Map.Util as MU

import MTG.Cards
import MTG.Pools

type MainEff a = Eff ( console :: CONSOLE
                     , fs      :: FS
                     , err     :: EXCEPTION
                     , random  :: RANDOM | a )

anyCard :: Cards -> Card -> Boolean
anyCard _ _ = true

p9 :: Cards -> Cards
p9 cs = MU.filterWithKey (\k _ -> 1 == (L.length $ L.filter ((==) k) p9_)) cs
  where
    p9_ = L.fromFoldable [ "Ancestral Recall"
                         , "Black Lotus"
                         , "Mox Pearl"
                         , "Mox Sapphire"
                         , "Mox Jet"
                         , "Mox Ruby"
                         , "Mox Emerald"
                         , "Time Walk"
                         , "Timetwister" ]

main :: forall e. (MainEff e) Unit
main = do
  x <- readTextFile UTF8 "~/.farseek/Cards.json"
  y <- readTextFile UTF8 "~/.farseek/Sets.json"
  let cards = (unsafePartial g $ mkCards x)
  let sets  = (unsafePartial g $ mkSets  y)
  singles  <- singletonNamesNames (L.replicate 4 anyCard) $ p9 cards
  log $ show $ singles
  where
    g :: forall a b. (Partial) => Either a b -> b
    g (Right x) = x
    g _         = unsafeCrashWith "Invalid JSON in ~/.farseek. Try running getJson.sh again."
