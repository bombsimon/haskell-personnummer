# haskell-personnummer

Validate Swedish [personal identity
numbers](<https://en.wikipedia.org/wiki/Personal_identity_number_(Sweden)>) with
[Haskell](https://www.haskell.org/)

## Usage

```haskell
import Personnummer (isValid, toPersonnummer)

pnrIsValid :: bool
pnrIsValid = isValid $ toPersonnummer "19900101-0007"
```

> **NOTE** To build this package you need to be able to build cabal-stack which
> (I think?) requires `llvm`. This can be a bit tricky if you're on macOS since
> `brew` won't link `llvm` if you have Xcode installed.
>
> One simple thing can be to just ensure the `brew` `llvm` directory is first
> in your `PATH` when using `cabal`.

```sh
PATH=$(brew --prefix)/opt/llvm/bin:$PATH cabal test
```

### Run example

```sh
cabal run Personnummer
```

### Test

```sh
cabal test
```

### Development

```sh
cabal repl
*Personnummer> :r
Ok, one module loaded.
```

## Formatter

[`ormolu`](https://github.com/tweag/ormolu)

If you're using VS Code, configure

```json
{
  "haskell.formattingProvider": "ormolu"
}
```
