## Module MTG.Pools

A module which provides means of making card pools.

#### `Slot`

``` purescript
type Slot = Card -> Boolean
```

#### `booster`

``` purescript
booster :: forall e. List Slot -> Cards -> Rnd e (Maybe Cards)
```

#### `boosterNames`

``` purescript
boosterNames :: forall e. List Slot -> Cards -> Rnd e (Maybe (List CardId))
```

#### `singleton`

``` purescript
singleton :: forall e. List Slot -> Cards -> Rnd e (Maybe (Tuple Cards Cards))
```

#### `singletonNames`

``` purescript
singletonNames :: forall e. List Slot -> Cards -> Rnd e (Maybe (Tuple (List CardId) Cards))
```

#### `singletonNamesNames`

``` purescript
singletonNamesNames :: forall e. List Slot -> Cards -> Rnd e (Maybe (Tuple (List CardId) (List CardId)))
```


