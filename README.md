# haskell-personnummer

Validate Swedish [personal identity
numbers](https://en.wikipedia.org/wiki/Personal_identity_number_(Sweden)) with
[Haskell](https://www.haskell.org/)

## Usage

```haskell
import Personnummer (fromString, isValid)

pnrIsValid :: bool
pnrIsValid = isValid . fromString "19900101-0007"
```

## Run example

```sh
cabal run Personnummer
```

## Test

```
cabal test
```

## Formatter

[`ormolu`](https://github.com/tweag/ormolu)
