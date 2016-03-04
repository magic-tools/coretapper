module Main where

import Control.Monad.Eff            (Eff)
import Control.Monad.Eff.Console    (CONSOLE, log)
import Control.Monad.Eff.Exception  (EXCEPTION())
import Data.Either                  (Either(..))
import Data.Argonaut.Parser         (jsonParser)
import Node.Encoding                (Encoding(UTF8))
import Node.FS                      (FS())
import Node.FS.Sync                 (readTextFile)
import Partial.Unsafe               (unsafePartial, unsafeCrashWith)
import Prelude

type MainEff a = Eff ( console :: CONSOLE
                     , fs      :: FS
                     , err     :: EXCEPTION | a )

main :: forall e. (MainEff e) Unit
main = do
  x <- readTextFile UTF8 "priv/AllSets.json"
  let y = (unsafePartial g <<< jsonParser) x
  (log <<< show) y
  where
    g :: forall a b. (Partial) => Either a b -> b
    g (Right x) = x
    g _         = unsafeCrashWith "Invalid JSON in priv/"
