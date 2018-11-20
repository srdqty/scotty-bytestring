# scotty-bytestring

[![Build Status](https://travis-ci.org/srdqty/scotty-bytestring.svg?branch=master)](https://travis-ci.org/srdqty/scotty-bytestring)

ByteString versions of some Scotty functions. The main purpose is to avoid
re-decoding and/or re-encoding data when reading and setting headers. It's
meant as an alternative for some of the functions that automatically decode
to Text.

## Example

Not the greatest example, since the string literals are overloaded, but it at
least demonstrates the functions that are available.

```
{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty hiding (header, headers, addHeader, setHeader, html)
import Web.Scotty.ByteString (header, headers, addHeader, setHeader, html)

import Data.Monoid (mconcat)

main = scotty 3000 $
  get "/:word" $ do
    beam <- param "word"

    someHeader <- header "SOME-HEADER"
    hs <- headers
    setHeader "THIS-HEADER" "header-value"

    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
```



## Development

I mostly use nix + cabal. I have tried nix + stack, and it worked on my
machine. Same with cabal new-\*. I haven't tested stack without nix, because
stack forces nix integration on NixOS.

### Nix + Cabal

See [nix/README.md](nix/README.md)

### Nix + Stack

```
stack build
stack test
```

### Cabal new-\*

```
cabal new-build
cabal new-test
```
### Stack

```
stack --no-nix build
stack --no-nix test
```
