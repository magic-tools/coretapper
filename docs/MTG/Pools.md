## Module MTG.Pools

A module which provides means of making card pools.

#### `Slot`

``` purescript
type Slot = Cards -> Card -> Boolean
```

Function type which takes a card candidate as the second argument, and

#### `booster`

``` purescript
booster :: forall e. List Slot -> Cards -> Rnd e (Maybe Cards)
```

Generates a booster from a pool of cards, based on List of Slots. Cards in booster may repeat.

#### `boosterNames`

``` purescript
boosterNames :: forall e. List Slot -> Cards -> Rnd e (Maybe (List CardId))
```

#### `singleton`

``` purescript
singleton :: forall e. List Slot -> Cards -> Rnd e (Maybe (Tuple Cards Cards))
```

Generates a pool with singlegon cards based on List of Slots. Cards which are picked are removed

#### `singletonNames`

``` purescript
singletonNames :: forall e. List Slot -> Cards -> Rnd e (Maybe (Tuple (List CardId) Cards))
```

#### `singletonNamesNames`

``` purescript
singletonNamesNames :: forall e. List Slot -> Cards -> Rnd e (Maybe (Tuple (List CardId) (List CardId)))
```


