<h1 align="center">miso-native</h1>
<p align="center">

<a href="https://native.haskell-miso.org">
  <img width=10% src="https://em-content.zobj.net/thumbs/240/apple/325/steaming-bowl_1f35c.png">
   </a>
<p align="center">A <i>tasty</i> <a href="https://www.haskell.org/"><strong>Haskell</strong></a> mobile framework 🍜</p>
</p>

<p align="center">
  <a href="https://matrix.to/#/#haskell-miso:matrix.org">
    <img src="https://img.shields.io/badge/matrix.org-miso-E01563.svg?style=flat-square" alt="Matrix #haskell-miso:matrix.org">
  </a>
  <a href="https://haskell.org">
    <img src="https://img.shields.io/badge/language-Haskell-orange.svg?style=flat-square" alt="Haskell">
  </a>
  <a href="https://haskell-miso-cachix.cachix.org">
    <img src="https://img.shields.io/badge/build-cachix-yellow.svg?style=flat-square" alt="Cachix">
  </a>
  <a href="https://actions-badge.atrox.dev/dmjio/miso/goto?ref=master">
    <img alt="Build Status" src="https://img.shields.io/endpoint.svg?url=https%3A%2F%2Factions-badge.atrox.dev%2Fdmjio%2Fmiso%2Fbadge%3Fref%3Dmaster&style=flat-square" />
  </a>
  <a href="http://hackage.haskell.org/package/miso-native">
    <img src="https://img.shields.io/hackage/v/miso-native.svg?style=flat-square" alt="Hackage">
  </a>
  <a href="https://github.com/dmjio/miso-native/blob/master/LICENSE">
    <img src="http://img.shields.io/badge/license-BSD3-blueviolet.svg?style=flat-square" alt="LICENSE">
  </a>
</p>

**Miso native** 🍜 is a small, production-ready, component-oriented, [isomorphic](http://nerds.airbnb.com/isomorphic-javascript-future-web-apps/) [Haskell](https://www.haskell.org/) mobile framework for quickly building highly interactive iOS and Android applications.

**Miso native** uses 🐈 [LynxJS](https://lynxjs.org) to facilitate drawing to native iOS `UIView` and Android views.

## Table of Contents
- [Quick Start](#quick-start)
- [Setup](#setup)
- [Hot Reload](#hot-reload)
- [Compilation](#compilation)
- [WebAssembly](#web-assembly)
- [JavaScript](#JavaScript)
- [Haddocks](#haddocks)
- [Architecture](#architecture)
- [Internals](#internals)
- [Examples](#examples)
- [Building examples](#building-examples)
- [HTTP](#interacting-with-http-apis)
- [Coverage](#coverage)
- [Isomorphic](#isomorphic)
- [Benchmarks](#benchmarks)
- [Nix](#nix)
  - [Pinning nixpkgs](#pinning-nixpkgs)
  - [Binary cache](#binary-cache)
- [Maintainers](#maintainers)
- [Contributing](#contributing)
- [Contributors](#contributors)
- [Partnerships](#partnerships)
- [Backers](#backers)
- [Organizations](#organizations)
- [License](#license)

## Quick start
To start developing applications with `miso-native` you will need to acquire [GHC](https://www.haskell.org/ghc/) and [cabal](https://www.haskell.org/cabal/). This can be done via [GHCup](https://www.haskell.org/ghcup/) or [Nix](https://nixos.org/).

> [!TIP]
> For new Haskell users we recommend using [GHCup](https://www.haskell.org/ghcup/) to acquire both [GHC](https://www.haskell.org/ghc/) and [cabal](https://www.haskell.org/cabal/)

## Setup

### `Main.hs`

This file contains a simple `miso-native` counter application.

```haskell
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE OverloadedStrings  #-}
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import           Miso hiding (id_)
import           Miso.Lens
import           Miso.String
import           Miso.Native
import qualified Miso.Style as CSS
-----------------------------------------------------------------------------
import           Prelude hiding (unlines)
-----------------------------------------------------------------------------
-- | Type synonym for an application model
newtype Model = Model { _value :: Int }
  deriving (Show, Eq)
-----------------------------------------------------------------------------
instance ToMisoString Model where
  toMisoString (Model v) = toMisoString v
-----------------------------------------------------------------------------
value :: Lens Model Int
value = lens _value $ \m v -> m { _value = v }
-----------------------------------------------------------------------------
data Action
  = AddOne
  | SubtractOne
  | SayHelloWorld
  | Tap Int
  deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | Entry point for a miso application
main :: IO ()
main = run $ native vcomp
  { events = nativeEvents
  , initialAction = Just SayHelloWorld
  }
-----------------------------------------------------------------------------
vcomp :: Component "counter" Model Action
vcomp = defaultComponent (Model 0) updateModel viewModel
-----------------------------------------------------------------------------
updateModel
  :: Action
  -> Effect Model Action
updateModel SayHelloWorld =
  io_ (consoleLog "Inside Say Hello World!")
updateModel AddOne = do
  io_ (consoleLog "Inside AddOne")
  value += 1
updateModel SubtractOne = do
  io_ (consoleLog "Inside SubtractOne")
  value -= 1
updateModel (Tap x) =
  io_ $ consoleLog ("tapped: " <> ms (show x))
-----------------------------------------------------------------------------
-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel m = view_
  [ CSS.style_
    [ CSS.height "200px"
    , CSS.display "flex"
    , CSS.alignItems "center"
    , CSS.justifyContent "center"
    ]
  ]
  [ view_
    [ onTap AddOne
    , id_ "plus"
    , CSS.style_
        [ CSS.backgroundColor CSS.yellow
        , CSS.width "100px"
        , CSS.height "100px"
        , CSS.margin "2px"
        , CSS.display "flex"
        , CSS.alignItems "center"
        , CSS.justifyContent "center"
        ]
    ]
    [ text_
      [ CSS.style_
        [ CSS.fontSize "48px"
        ]
      ]
      [ "🐈"
      ]
    ]
  , view_
    [ CSS.style_
        [ CSS.backgroundColor CSS.orange
        , CSS.width "100px"
        , CSS.height "100px"
        , CSS.display "flex"
        , CSS.alignItems "center"
        , CSS.justifyContent "center"
        ]
    ]
    [ text_
      [ CSS.style_
        [ CSS.fontSize "48px"
        ]
      ]
      [ text $ ms (m ^. value)
      ]
    ]
  , view_
    [ onTap SubtractOne
    , id_ "minus"
    , CSS.style_
        [ CSS.backgroundColor CSS.pink
        , CSS.width "100px"
        , CSS.height "100px"
        , CSS.margin "2px"
        , CSS.display "flex"
        , CSS.alignItems "center"
        , CSS.justifyContent "center"
        ]
    ]
    [ text_
      [ CSS.style_
        [ CSS.fontSize "48px"
        ]
      ]
      [ "🍜"
      ]
    ]
 ]


```

Now that your project files are populated, development can begin.

## Hot Reload

Coming soon! 

## Compilation

Coming soon ! 


## JavaScript
Coming soon !

## Haddocks

Coming soon !

## Architecture
Coming soon !


## Internals ⚙️
Coming soon ! 

## Examples

Coming soon !

## Building examples

Coming soon !

## Interacting with HTTP APIs 🔌

Coming soon !

## Coverage ✅
Coming soon !

## Isomorphic ☯️

Coming soon !

## Benchmarks 🏎️

Coming soon !

### Pinning nixpkgs 📌

Coming soon !

### Binary cache

`nix` users on a Linux or OSX distros can take advantage of a [binary cache](https://haskell-miso-cachix.cachix.org) for faster builds. To use the binary cache follow the instructions on [cachix](https://haskell-miso-cachix.cachix.org/).

> [!TIP]
> We highly recommend nix users consume the [cachix](https://cachix.org) cache. `cachix use haskell-miso-cachix`.

```bash
$ cachix use haskell-miso-cachix
```

## Maintainers

[@dmjio](https://github.com/dmjio)

## Contributing

Feel free to dive in! [Open an issue](https://github.com/dmjio/miso-native/issues/new) or a submit [Pull Request](https://github.com/dmjio/miso-native/pulls).

See [CONTRIBUTING](https://github.com/dmjio/miso-native/blob/master/CONTRIBUTING.md) for more info.

## Contributors 🦾

> [!NOTE]
> This project exists thanks to all the people who [contribute](CONTRIBUTING.md)

<a href="https://github.com/dmjio/miso-native/graphs/contributors"><img src="https://opencollective.com/miso/contributors.svg?width=890&button=false" /></a>

## Partnerships 🤝

If you'd like to support this project financially, be it through requesting feature development, or a corporate partnership, please drop us a line and we will be in touch shortly. <p><a href="mailto:code@dmj.io">code@dmj.io</a></p>

## Backers

Become a [financial contributor](https://opencollective.com/miso/contribute) and help us sustain our project and community. We are very grateful and thankful for our individual sponsors.

  - Moses Tschanz
  - [@MaxGabriel](https://github.com/MaxGabriel)
  - [@DigitalOcean](https://github.com/DigitalOcean)
  - [@maybetonyfu](https://github.com/maybetonyfu)
  - [@jhrcek](https://github.com/jhrcek)
  - etc.

<a href="https://opencollective.com/miso"><img src="https://opencollective.com/miso/individuals.svg?width=890"></a>

## Organizations

[Support this project](https://opencollective.com/miso/contribute) with your organization. Your logo will show up here with a link to your website. We are also very grateful and thankful for our corporate sponsors.

<a target="_blank" href="https://opencollective.com/miso/organization/0/website"><img src="https://opencollective.com/miso/organization/0/avatar.svg"></a>

## License

[BSD3](LICENSE) © dmjio
