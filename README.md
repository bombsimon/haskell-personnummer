# haskell-personnummer

Validate Swedish [personal identity
numbers](<https://en.wikipedia.org/wiki/Personal_identity_number_(Sweden)>) with
[Haskell](https://www.haskell.org/)

## Usage

```haskell
import Personnummer (isValid, toPersonnummer)

pnrIsValid :: Bool
pnrIsValid = isValid $ toPersonnummer "19900101-0017"
```

> **NOTE** To build this package with `llvm` backend which cabal defaults to
> (I think?) on macOS you might need some changes since `brew` won't link
> `llvm` if you have Xcode installed.
>
> One simple thing can be to just ensure the `brew` `llvm` directory is first
> in your `PATH` when using `cabal`.

```sh
PATH=$(brew --prefix)/opt/llvm/bin:$PATH cabal test
```

Or just specify the compiler with `--ghc-option=-f[llvm|asm]`

### Run example

```sh
cabal run Personnummer --ghc-option=-fllvm
```

### Test

```sh
cabal test --ghc-option=-fllvm
```

### Development

```sh
$ cabal repl
*Personnummer> let pnr = toPersonnummer "9001010017"
*Personnummer> isValid pnr
True
*Personnummer> gender pnr
Male
*Personnummer> format pnr True
"19900101-0017"
```

## Formatter

[`ormolu`](https://github.com/tweag/ormolu)

If you're using VS Code, configure

```json
{
  "haskell.formattingProvider": "ormolu"
}
```
