<h1 align="center">miso-lynx</h1>
<p align="center">

<a href="https://native.haskell-miso.org">
<p align="center">A <i>tasty</i> <a href="https://www.haskell.org/"><strong>Haskell</strong></a> mobile framework 🍜 🐈 📱 </p>
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
  <a href="https://actions-badge.atrox.dev/haskell-miso/miso-lynx/goto?ref=master">
    <img alt="Build Status" src="https://img.shields.io/endpoint.svg?url=https%3A%2F%2Factions-badge.atrox.dev%2Fhaskell-miso%2Fmiso-lynx%2Fbadge%3Fref%3Dmaster&style=flat-square" />
  </a>
  <a href="http://hackage.haskell.org/package/miso-lynx">
    <img src="https://img.shields.io/hackage/v/miso-lynx.svg?style=flat-square" alt="Hackage">
  </a>
  <a href="https://github.com/haskell-miso/miso-lynx/blob/master/LICENSE">
    <img src="http://img.shields.io/badge/license-BSD3-blueviolet.svg?style=flat-square" alt="LICENSE">
  </a>
</p>

##

**Miso Lynx** 🍜 🐈 is a mobile framework that uses [miso](https://github.com/dmjio/miso) and [LynxJS](https://lynxjs.org) to facilitate drawing to native iOS [UIView](https://developer.apple.com/documentation/uikit/uiview), Android [View](https://developer.android.com/reference/android/view/View), and for interacting with APIs on the device. The [Haskell miso project](https://github.com/haskell-miso) is excited to be an open-source technology partner with innovative technology companies like [ByteDance](https://github.com/lynx-family), creators of [LynxJS](https://lynxjs.org/), to advance native mobile app development in the functional programming space.

## Why Lynx

[Since 2017](https://github.com/dmjio/miso/issues/314), `miso` has sought ways to run on mobile devices that take advantage of native drawing facilities. [Flutter](https://flutter.dev/), [SwiftUI](https://developer.apple.com/swiftui/) produce stunning, performant user experiences but are not necessarily designed for integration with open standards, are effectively only usable with a specific IDE / choice languages, and can lack in cross-platform capability. [React Native](https://reactnative.dev/) improves upon this situation by allowing open web standards for development and cross-platform capability, but wasn't designed for easy integration with ***any*** web framework (rather for use specifically with [react](https://react.dev)). [React Native](https://reactnative.dev/) also can regress in performance relative to `SwiftUI` / `Flutter` in certain scenarios.

[Lynx](https://lynxjs.org/) addresses the aforementioned issues with a new architectural approach.

 - #### Dual-threaded interpreter runtime
   The [Lynx](https://lynxjs.org) engine uses [two embedded JavaScript interpreters](https://lynxjs.org/react/thinking-in-reactlynx.html#your-code-runs-on-two-threads) to selectively schedule / offload computation to free up the render thread. This avoids drawing lag as commonly seen with scroll events in react native applications.

 - #### Element PAPI (PrimJS API)
   Exposing a [DOM API](https://lynxjs.org/api/engine/element-api.html) for rendering allows any JavaScript (or compile-to-JavaScript 🍜) web framework to produce cross-platfom mobile applications. [Seen here](https://github.com/haskell-miso/miso-lynx/blob/master/ts/miso/context/lynx.ts) in `miso-lynx`.

 - #### Cross platform capability
   Lynx targets iOS, Android, Web by default, and has [a roadmap](https://lynxjs.org/blog/lynx-open-source-roadmap-2025) that mentions Desktop UI as well (OSX, etc.)

 - #### Instant first-frame rendering - [IFR](https://lynxjs.org/guide/interaction/ifr.html#ifr-is-one-of-the-advantages-of-lynx)
   The flagship isomorphic (server side rendering) feature in `miso` can be repurposed as IFR in `miso-lynx` (one of the flagship features of [lynx](https://lynxjs.org)).

For framework implementors, this is a dream come true, and we hope `miso-lynx` can be an ideal development environment for building [Lynx](https://lynxjs.org) applications with [miso](https://haskell-miso.org).

## Table of Contents
- [React Summit](#react-summit)
- [Fireship](#fireship)
- [Demo](#demo)
- [Preview](#preview)
- [Quick Start](#quick-start)
- [Setup](#setup)
- [Hot Reload](#hot-reload)
- [Binary cache](#binary-cache)
- [Maintainers](#maintainers)
- [Contributing](#contributing)
- [License](#license)

## React Summit

As seen @ [React Summit](https://reactsummit.com/) by [@huxpro](https://github.com/huxpro) !

[![Alt text](https://img.youtube.com/vi/l2dByiwiQcM/0.jpg)](https://www.youtube.com/watch?v=l2dByiwiQcM)
 
The Haskell miso portion is queued [here](https://youtu.be/l2dByiwiQcM?si=3IghUTRryYAyb7SK&t=1712).

## Fireship

See [Fireship](https://www.youtube.com/watch?v=-qjE8JkIVoQ) 🔥 video

## Demo

<img src="https://github.com/user-attachments/assets/385d8686-849c-4be8-99d9-f43a40f5fb43" width="40%" />

## Preview

To run the example locally execute the following command

```bash
$ git clone git@github.com:haskell-miso/miso-lynx.git
$ http-server ./miso-lynx/examples
```

This will host the `main.lynx.bundle` which can be loaded into the `LynxExplorer` for interactive development.

> [!NOTE] 
> You will need to have the LynxExplorer installed which works with the iOS simulator. Please see the [LynxJS](https://lynxjs.org) getting started guide for installation.

## Quick Start

> [!WARNING]
> `miso-lynx` depends on the latest version of `miso` (version `1.9`), this includes custom renderers (ala React Renderer) and Components as well.
> Currently all event handling and drawing are performed on the main thread. Selectively scheduling Haskell code on the Lynx MTS / BTS is ongoing research.
> This project is under heavy development and is considered experimental.

To start developing applications with `miso-lynx` you will need to acquire [GHC](https://www.haskell.org/ghc/) and [cabal](https://www.haskell.org/cabal/). This can be done via [GHCup](https://www.haskell.org/ghcup/) or [Nix](https://nixos.org/).

> [!TIP]
> For new Haskell users we recommend using [GHCup](https://www.haskell.org/ghcup/) to acquire both [GHC](https://www.haskell.org/ghc/) and [cabal](https://www.haskell.org/cabal/)

## Setup

### `Main.hs`

This file contains a simple `miso-lynx` counter application.

```haskell
-----------------------------------------------------------------------------
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
main = run $ lynx counterComponent
  { events = lynxEvents
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
  io_ (consoleLog "Hello World!")
updateModel AddOne =
  value += 1
updateModel SubtractOne =
  value -= 1
updateModel (Tap x) =
  io_ $ consoleLog ("Tapped: " <> ms (show x))
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

This entails creating a [LynxExplorer](https://lynxjs.org) application with the [jsaddle-warp](https://hackage.haskell.org/package/jsaddle-warp) hot-reload package. This will require using [rspack](https://rspack.rs/) and the BTS to access WebSockets via the `lynx` object.

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

Feel free to dive in! [Open an issue](https://github.com/dmjio/miso-lynx/issues/new) or a submit [Pull Request](https://github.com/dmjio/miso-lynx/pulls).

See [CONTRIBUTING](https://github.com/dmjio/miso-lynx/blob/master/CONTRIBUTING.md) for more info.

## License

[BSD3](LICENSE) © dmjio
