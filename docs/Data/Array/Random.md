## Module Data.Array.Random

A module which provides a naive and suboptimal way of shuffling arrays

#### `Rnd`

``` purescript
type Rnd e = Eff (random :: RANDOM | e)
```

#### `shuffle`

``` purescript
shuffle :: forall a e. Array a -> Rnd e (Array a)
```

Shuffles an array. Slow as fuck. TODO: refactor

#### `choose`

``` purescript
choose :: forall a e. Array a -> Rnd e a
```

Chooses a random value in an array.

#### `pop`

``` purescript
pop :: forall a e. Array a -> Rnd e (Tuple a (Array a))
```

Chooses a random value and pops it out. Returns the chosen


