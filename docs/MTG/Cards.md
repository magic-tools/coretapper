## Module MTG.Cards

A module providing ways to work with Magic cards.

#### `mkCards`

``` purescript
mkCards :: String -> F Cards
```

#### `mkSets`

``` purescript
mkSets :: String -> F Sets
```

#### `Cards`

``` purescript
type Cards = Map CardId Card
```

#### `Sets`

``` purescript
type Sets = Map SetId Set
```

#### `CompRarity`

``` purescript
type CompRarity = Either (Array String) String
```

Sometimes Rarity is [ "mythic rare", "rare" ], hence `Left (Array String)` option

#### `Rarity`

``` purescript
type Rarity = String
```

#### `CardType`

``` purescript
type CardType = String
```

#### `Color`

``` purescript
type Color = String
```

#### `Cost`

``` purescript
type Cost = String
```

#### `Power`

``` purescript
type Power = String
```

#### `Subtype`

``` purescript
type Subtype = String
```

#### `Toughness`

``` purescript
type Toughness = String
```

#### `Loyalty`

``` purescript
type Loyalty = String
```

#### `CardId`

``` purescript
type CardId = String
```

#### `SetId`

``` purescript
type SetId = String
```

#### `Card`

``` purescript
data Card
  = Card { name :: CardId, rarity :: Maybe Rarity, oracle :: Maybe String, manaCost :: Maybe Cost, cmc :: Maybe Number, types :: Maybe (Array CardType), subtypes :: Maybe (Array Subtype), colors :: Maybe (Array Color), colorRelation :: Maybe (Array Color), power :: Maybe Power, toughness :: Maybe Toughness, loyalty :: Maybe Loyalty }
```

##### Instances
``` purescript
Generic Card
Show Card
Eq Card
Ord Card
IsForeign Card
```

#### `Set`

``` purescript
data Set
  = Set { cards :: Array CardId, code :: SetId, name :: Maybe String }
```

##### Instances
``` purescript
Generic Set
Show Set
Eq Set
Ord Set
IsForeign Set
```


