-- | A module which provides a naive and suboptimal way of shuffling lists
-- and accessing a random element of list.
module Data.List.Random ( shuffle
                         , choose
                         , pop
                         , Rnd(..) ) where

import Prelude                        (($), pure, (-), bind)
import Control.Monad.Eff              (Eff)
import Control.Monad.Eff.Random       (RANDOM, randomInt)
import Data.List
import Data.Maybe                     (Maybe)
import Data.Maybe.Unsafe              (fromJust)
import Data.Tuple                     (Tuple(Tuple))

type Rnd e = Eff ( random :: RANDOM | e )

-- | Shuffles an list. Slow as fuck. TODO: refactor
shuffle :: forall a e. List a -> (Rnd e) (List a)
shuffle xs = shuffleDo xs (length xs)

shuffleDo :: forall a e. List a -> Int -> (Rnd e) (List a)
shuffleDo acc 0 = pure acc
shuffleDo acc n = do
  r <- randomInt 0 $ (length acc) - 1
  let a = fj $ acc !! (n - 1)
  let b = fj $ acc !! r
  let acc1 = fj $ updateAt (n - 1) b acc
  let acc2 = fj $ updateAt r       a acc1
  shuffleDo acc2 (n - 1)

-- | Chooses a random value in an list.
choose :: forall a e. List a -> (Rnd e) a
choose xs = do
  r <- randomInt 0 $ (length xs) - 1
  pure $ fj $ xs !! r

-- | Chooses a random value and pops it out. Returns the chosen
-- value as the first element of the Tuple and resulting list
-- without the chosen element as the second.
pop :: forall a e. List a -> (Rnd e) (Tuple a (List a))
pop xs = do
  r <- randomInt 0 $ (length xs) - 1
  pure $ Tuple (fj $ xs !! r) (fj $ deleteAt r xs)

fj :: forall a. Maybe a -> a
fj = fromJust
