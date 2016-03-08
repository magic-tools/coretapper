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

import Data.Foreign
import Data.Foreign.Class

import Data.Map as M

import MTG.Cards

type MainEff a = Eff ( console :: CONSOLE
                     , fs      :: FS
                     , err     :: EXCEPTION | a )

main :: forall e. (MainEff e) Unit
main = do
  x <- readTextFile UTF8 "priv/Cards.json"
  y <- readTextFile UTF8 "priv/Sets.json"
  let cards = readJSON x :: F Cards
  let sets  = readJSON y :: F Sets
  log $ show cards
  log $ show sets
  where
    g :: forall a b. (Partial) => Either a b -> b
    g (Right x) = x
    g _         = unsafeCrashWith "Invalid JSON in priv/"
