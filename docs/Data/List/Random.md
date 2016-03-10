## Module Data.List.Random

A module which provides a naive and suboptimal way of shuffling lists

#### `Rnd`

``` purescript
type Rnd e = Eff (random :: RANDOM | e)
```

#### `shuffle`

``` purescript
shuffle :: forall a e. List a -> Rnd e (List a)
```

Shuffles an list. Slow as fuck. TODO: refactor

#### `choose`

``` purescript
choose :: forall a e. List a -> Rnd e a
```

Chooses a random value in an list.

#### `pop`

``` purescript
pop :: forall a e. List a -> Rnd e (Tuple a (List a))
```

Chooses a random value and pops it out. Returns the chosen


