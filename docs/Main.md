## Module Main

#### `MainEff`

``` purescript
type MainEff a = Eff (console :: CONSOLE, fs :: FS, err :: EXCEPTION, random :: RANDOM | a)
```

#### `anyCard`

``` purescript
anyCard :: Card -> Boolean
```

#### `p9`

``` purescript
p9 :: Cards -> Cards
```

#### `main`

``` purescript
main :: forall e. MainEff e Unit
```


