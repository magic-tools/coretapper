## Module Data.Map.Util

A module exporting some of the missing utils from Data.Map.

#### `filter`

``` purescript
filter :: forall k v. (Ord k) => (v -> Boolean) -> Map k v -> Map k v
```

#### `filterWithKey`

``` purescript
filterWithKey :: forall k v. (Ord k) => (k -> v -> Boolean) -> Map k v -> Map k v
```


