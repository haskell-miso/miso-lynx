<h1 align="center">miso-native</h1>
<p align="center">

<a href="https://native.haskell-miso.org">
<p align="center">A <i>tasty</i> <a href="https://www.haskell.org/"><strong>Haskell</strong></a> mobile framework 🍜 🐈 </p>
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

**Miso Lynx** 🍜 is a mobile framework that uses [miso](https://github.com/dmjio/miso) and [LynxJS](https://lynxjs.org) 🐈 for building iOS and Android applications. **Miso Lynx** uses [LynxJS](https://lynxjs.org) 🐈 to facilitate drawing to native iOS [UIView](https://developer.apple.com/documentation/uikit/uiview) and Android [ViewGroup](https://developer.android.com/reference/android/view/ViewGroup), and for interacting with APIs on the device.

## Table of Contents
## Table of Contents
- [Demo](#demo)
- [Quick Start](#quick-start)
- [Setup](#setup)
- [Hot Reload](#hot-reload)
- [Compilation](#compilation)
- [Haddocks](#haddocks)
- [Nix](#nix)
  - [Pinning nixpkgs](#pinning-nixpkgs)
  - [Binary cache](#binary-cache)
- [Maintainers](#maintainers)
- [Contributing](#contributing)
- [License](#license)

## Demo

Demo video goes here

## Preview

To run the example locally execute the following

```bash
$ git clone git@github.com:dmjio/miso-lynx.git
$ http-server ./miso-lynx/examples
```

This will host the `main.lynx.bundle` which can be loaded into the `LynxExplorer` for interactive development.


## Quick start
To start developing applications with `miso-native` you will need to acquire [GHC](https://www.haskell.org/ghc/) and [cabal](https://www.haskell.org/cabal/). This can be done via [GHCup](https://www.haskell.org/ghcup/) or [Nix](https://nixos.org/).

> [!TIP]
> For new Haskell users we recommend using [GHCup](https://www.haskell.org/ghcup/) to acquire both [GHC](https://www.haskell.org/ghc/) and [cabal](https://www.haskell.org/cabal/)

## Setup

### `Main.hs`

This file contains a simple `miso-native` counter application.

```haskell
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import           Miso
import           Miso.Lynx
-----------------------------------------------------------------------------
import           Miso.Lens
import           Miso.String
import qualified Miso.Style as CSS
-----------------------------------------------------------------------------
-- | Type synonym for an application model
newtype Model = Model { _value :: Int }
  deriving (Show, Eq, ToMisoString)
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
main = run $ native counterComponent
  { events = nativeEvents
  , initialAction = Just SayHelloWorld
  }
-----------------------------------------------------------------------------
counterComponent :: Component Model Action
counterComponent = component (Model 0) updateModel viewModel
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
-----------------------------------------------------------------------------
```

Now that your project files are populated, development can begin.

## Hot Reload

This entails creating a [LynxExplorer](https://lynxjs.org) application with the [jsaddle-warp](https://hackage.haskell.org/package/jsaddle-warp) hot-reload package from [@hamishmack](https://github.com/hamishmack).

## Compilation

Coming soon ! 

## Haddocks

```bash
$ cabal haddock-project
```

 - [GHC](https://github.com/hamishmack)

## Examples

- [Examples](https://github.com/dmjio/miso-lynx/tree/master/examples)

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

## License

[BSD3](LICENSE) © dmjio
