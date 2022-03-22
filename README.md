# haskell-personnummer

[![Test and lint](https://github.com/bombsimon/haskell-personnummer/actions/workflows/haskell.yml/badge.svg)](https://github.com/bombsimon/haskell-personnummer/actions/workflows/haskell.yml)

Validate Swedish [personal identity
numbers](<https://en.wikipedia.org/wiki/Personal_identity_number_(Sweden)>) with
[Haskell](https://www.haskell.org/)

## Usage

```haskell
import Personnummer (isValid, toPersonnummer)

pnrIsValid :: Bool
pnrIsValid = isValid $ fromJust toPersonnummer "19900101-0017"
```

### Run example

```sh
PATH=$(brew --prefix)/opt/llvm/bin:$PATH cabal run Personnummer  \
  --ghc-option=-fllvm
```

### REPL

```sh
$ cabal repl
*Personnummer> isValidString "199001010018"
Fasle
```

```sh
$ cabal repl
*Personnummer> let pnr = fromJust $ toPersonnummer "9001010017"
*Personnummer> isValid pnr
True
*Personnummer> gender pnr
Male
*Personnummer> isFemale pnr
False
*Personnummer> format pnr True
"19900101-0017"
*Personnummer> getAge pnr
32
```

### Test

```sh
cabal test --ghc-option=-fllvm
```

## Formatter

[`ormolu`](https://github.com/tweag/ormolu)

If you're using VS Code, configure

```json
{
  "haskell.formattingProvider": "ormolu"
}
```
