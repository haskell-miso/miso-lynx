-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.Style
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
-----------------------------------------------------------------------------
module Miso.Native.Style
  ( -- *** Types
    Style
    -- *** Combinators
  , alignContent
  , alignItems
  , alignSelf
  , animationDelay
  , animationDirection
  , animationDuration
  , animationFillMode
  , animationIterationCount
  , animation
  , animationName
  , animationPlayState
  , animationTimingFunction
  , aspectRatio
  , backgroundClip
  , backgroundColor
  , backgroundImage
  , background
  , backgroundOrigin
  , backgroundPosition
  , backgroundRepeat
  , backgroundSize
  , borderBottomColor
  , borderBottomLeftRadius
  , borderBottom
  , borderBottomRightRadius
  , borderBottomStyle
  , borderBottomWidth
  , borderColor
  , borderEndEndRadius
  , borderEndStartRadius
  , borderInlineEndColor
  , borderInlineEndStyle
  , borderInlineEndWidth
  , borderInlineStartColor
  , borderInlineStartStyle
  , borderInlineStartWidth
  , borderLeftColor
  , borderLeft
  , borderLeftStyle
  , borderLeftWidth
  , border
  , borderRadius
  , borderRightColor
  , borderRight
  , borderRightStyle
  , borderRightWidth
  , borderStartEndRadius
  , borderStartStartRadius
  , borderStyle
  , borderTopColor
  , borderTopLeftRadius
  , borderTop
  , borderTopRightRadius
  , borderTopStyle
  , borderTopWidth
  , borderWidth
  , bottom
  , boxShadow
  , boxSizing
  , clipPath
  , color
  , columnGap
  , cssVariable
  , direction
  , display
  , filter
  , flexBasis
  , flexDirection
  , flexFlow
  , flexGrow
  , flex
  , flexShrink
  , flexWrap
  , fontFamily
  , fontSize
  , fontStyle
  , fontWeight
  , gap
  , gridAutoColumns
  , gridAutoFlow
  , gridAutoRows
  , gridColumnEnd
  , gridColumnSpan
  , gridColumnStart
  , gridRowEnd
  , gridRowSpan
  , gridRowStart
  , gridTemplateColumns
  , gridTemplateRows
  , height
  , imageRendering
  , insetInlineEnd
  , insetInlineStart
  , justifyContent
  , justifyItems
  , justifySelf
  , left
  , letterSpacing
  , linearCrossGravity
  , linearDirection
  , linearGravity
  , linearLayoutGravity
  , linearWeight
  , linearWeightSum
  , lineHeight
  , marginBottom
  , marginInlineEnd
  , marginInlineStart
  , marginLeft
  , margin
  , marginRight
  , marginTop
  , maskImage
  , mask
  , maxHeight
  , maxWidth
  , minHeight
  , minWidth
  , opacity
  , order
  , overflow
  , overflowX
  , overflowY
  , paddingBottom
  , paddingInlineEnd
  , paddingInlineStart
  , paddingLeft
  , padding
  , paddingRight
  , paddingTop
  , perspective
  , position
  , relativeAlignBottom
  , relativeAlignInlineEnd
  , relativeAlignInlineStart
  , relativeAlignLeft
  , relativeAlignRight
  , relativeAlignTop
  , relativeBottomOf
  , relativeCenter
  , relativeId
  , relativeInlineEndOf
  , relativeInlineStartOf
  , relativeLayoutOnce
  , relativeLeftOf
  , relativeRightOf
  , relativeTopOf
  , right
  , rowGap
  , textAlign
  , textDecoration
  , textIndent
  , textOverflow
  , textShadow
  , textStrokeColor
  , textStroke
  , textStrokeWidth
  , top
  , transform
  , transformOrigin
  , transitionDelay
  , transitionDuration
  , transition
  , transitionProperty
  , transitionTimingFunction
  , verticalAlign
  , visibility
  , whiteSpace
  , width
  , wordBreak
  , xAutoFontSize
  , xAutoFontSizePresetSizes
  , xHandleColor
  , xHandleSize
  , zIndex
  ) where
-----------------------------------------------------------------------------
import Miso.Util ((===))
import Miso.String (MisoString)
-----------------------------------------------------------------------------
import Prelude hiding (filter)
-----------------------------------------------------------------------------
-- | 'Style'
--
-- Type for a CSS style on native
--
type Style = (MisoString, MisoString)
-----------------------------------------------------------------------------
-- alignContent
--
-- > styles_ $ M.fromList [ alignContent .= "value" ]
--
alignContent :: MisoString -> Style
alignContent x = "align-content" === x
-----------------------------------------------------------------------------
-- alignItems
--
-- > styles_ $ M.fromList [ alignItems .= "value" ]
--
alignItems :: MisoString -> Style
alignItems x = "align-items" === x
-----------------------------------------------------------------------------
-- alignSelf
--
-- > styles_ $ M.fromList [ alignSelf .= "value" ]
--
alignSelf :: MisoString -> Style
alignSelf x = "align-self" === x
-----------------------------------------------------------------------------
-- animationDelay
--
-- > styles_ $ M.fromList [ animationDelay .= "value" ]
--
animationDelay :: MisoString -> Style
animationDelay x = "animation-delay" === x
-----------------------------------------------------------------------------
-- animationDirection
--
-- > styles_ $ M.fromList [ animationDirection .= "value" ]
--
animationDirection :: MisoString -> Style
animationDirection x = "animation-direction" === x
-----------------------------------------------------------------------------
-- animationDuration
--
-- > styles_ $ M.fromList [ animationDuration .= "value" ]
--
animationDuration :: MisoString -> Style
animationDuration x = "animation-duration" === x
-----------------------------------------------------------------------------
-- animationFillMode
--
-- > styles_ $ M.fromList [ animationFillMode .= "value" ]
--
animationFillMode :: MisoString -> Style
animationFillMode x = "animation-fill-mode" === x
-----------------------------------------------------------------------------
-- animationIterationCount
--
-- > styles_ $ M.fromList [ animationIterationCount .= "value" ]
--
animationIterationCount :: MisoString -> Style
animationIterationCount x = "animation-iteration-count" === x
-----------------------------------------------------------------------------
-- animation
--
-- > styles_ $ M.fromList [ animation .= "value" ]
--
animation :: MisoString -> Style
animation x = "animation" === x
-----------------------------------------------------------------------------
-- animationName
--
-- > styles_ $ M.fromList [ animationName .= "value" ]
--
animationName :: MisoString -> Style
animationName x = "animation-name" === x
-----------------------------------------------------------------------------
-- animationPlayState
--
-- > styles_ $ M.fromList [ animationPlayState .= "value" ]
--
animationPlayState :: MisoString -> Style
animationPlayState x = "animation-play-state" === x
-----------------------------------------------------------------------------
-- animationTimingFunction
--
-- > styles_ $ M.fromList [ animationTimingFunction .= "value" ]
--
animationTimingFunction :: MisoString -> Style
animationTimingFunction x = "animation-timing-function" === x
-----------------------------------------------------------------------------
-- aspectRatio
--
-- > styles_ $ M.fromList [ aspectRatio .= "value" ]
--
aspectRatio :: MisoString -> Style
aspectRatio x = "aspect-ratio" === x
-----------------------------------------------------------------------------
-- backgroundClip
--
-- > styles_ $ M.fromList [ backgroundClip .= "value" ]
--
backgroundClip :: MisoString -> Style
backgroundClip x = "background-clip" === x
-----------------------------------------------------------------------------
-- backgroundColor
--
-- > styles_ $ M.fromList [ backgroundColor .= "value" ]
--
backgroundColor :: MisoString -> Style
backgroundColor x = "background-color" === x
-----------------------------------------------------------------------------
-- backgroundImage
--
-- > styles_ $ M.fromList [ backgroundImage .= "value" ]
--
backgroundImage :: MisoString -> Style
backgroundImage x = "background-image" === x
-----------------------------------------------------------------------------
-- background
--
-- > styles_ $ M.fromList [ background .= "value" ]
--
background :: MisoString -> Style
background x = "background" === x
-----------------------------------------------------------------------------
-- backgroundOrigin
--
-- > styles_ $ M.fromList [ backgroundOrigin .= "value" ]
--
backgroundOrigin :: MisoString -> Style
backgroundOrigin x = "background-origin" === x
-----------------------------------------------------------------------------
-- backgroundPosition
--
-- > styles_ $ M.fromList [ backgroundPosition .= "value" ]
--
backgroundPosition :: MisoString -> Style
backgroundPosition x = "background-position" === x
-----------------------------------------------------------------------------
-- backgroundRepeat
--
-- > styles_ $ M.fromList [ backgroundRepeat .= "value" ]
--
backgroundRepeat :: MisoString -> Style
backgroundRepeat x = "background-repeat" === x
-----------------------------------------------------------------------------
-- backgroundSize
--
-- > styles_ $ M.fromList [ backgroundSize .= "value" ]
--
backgroundSize :: MisoString -> Style
backgroundSize x = "background-size" === x
-----------------------------------------------------------------------------
-- borderBottomColor
--
-- > styles_ $ M.fromList [ borderBottomColor .= "value" ]
--
borderBottomColor :: MisoString -> Style
borderBottomColor x = "border-bottom-color" === x
-----------------------------------------------------------------------------
-- borderBottomLeftRadius
--
-- > styles_ $ M.fromList [ borderBottomLeftRadius .= "value" ]
--
borderBottomLeftRadius :: MisoString -> Style
borderBottomLeftRadius x = "border-bottom-left-radius" === x
-----------------------------------------------------------------------------
-- borderBottom
--
-- > styles_ $ M.fromList [ borderBottom .= "value" ]
--
borderBottom :: MisoString -> Style
borderBottom x = "border-bottom" === x
-----------------------------------------------------------------------------
-- borderBottomRightRadius
--
-- > styles_ $ M.fromList [ borderBottomRightRadius .= "value" ]
--
borderBottomRightRadius :: MisoString -> Style
borderBottomRightRadius x = "border-bottom-right-radius" === x
-----------------------------------------------------------------------------
-- borderBottomStyle
--
-- > styles_ $ M.fromList [ borderBottomStyle .= "value" ]
--
borderBottomStyle :: MisoString -> Style
borderBottomStyle x = "border-bottom-style" === x
-----------------------------------------------------------------------------
-- borderBottomWidth
--
-- > styles_ $ M.fromList [ borderBottomWidth .= "value" ]
--
borderBottomWidth :: MisoString -> Style
borderBottomWidth x = "border-bottom-width" === x
-----------------------------------------------------------------------------
-- borderColor
--
-- > styles_ $ M.fromList [ borderColor .= "value" ]
--
borderColor :: MisoString -> Style
borderColor x = "border-color" === x
-----------------------------------------------------------------------------
-- borderEndEndRadius
--
-- > styles_ $ M.fromList [ borderEndEndRadius .= "value" ]
--
borderEndEndRadius :: MisoString -> Style
borderEndEndRadius x = "border-end-end-radius" === x
-----------------------------------------------------------------------------
-- borderEndStartRadius
--
-- > styles_ $ M.fromList [ borderEndStartRadius .= "value" ]
--
borderEndStartRadius :: MisoString -> Style
borderEndStartRadius x = "border-end-start-radius" === x
-----------------------------------------------------------------------------
-- borderInlineEndColor
--
-- > styles_ $ M.fromList [ borderInlineEndColor .= "value" ]
--
borderInlineEndColor :: MisoString -> Style
borderInlineEndColor x = "border-inline-end-color" === x
-----------------------------------------------------------------------------
-- borderInlineEndStyle
--
-- > styles_ $ M.fromList [ borderInlineEndStyle .= "value" ]
--
borderInlineEndStyle :: MisoString -> Style
borderInlineEndStyle x = "border-inline-end-style" === x
-----------------------------------------------------------------------------
-- borderInlineEndWidth
--
-- > styles_ $ M.fromList [ borderInlineEndWidth .= "value" ]
--
borderInlineEndWidth :: MisoString -> Style
borderInlineEndWidth x = "border-inline-end-width" === x
-----------------------------------------------------------------------------
-- borderInlineStartColor
--
-- > styles_ $ M.fromList [ borderInlineStartColor .= "value" ]
--
borderInlineStartColor :: MisoString -> Style
borderInlineStartColor x = "border-inline-start-color" === x
-----------------------------------------------------------------------------
-- borderInlineStartStyle
--
-- > styles_ $ M.fromList [ borderInlineStartStyle .= "value" ]
--
borderInlineStartStyle :: MisoString -> Style
borderInlineStartStyle x = "border-inline-start-style" === x
-----------------------------------------------------------------------------
-- borderInlineStartWidth
--
-- > styles_ $ M.fromList [ borderInlineStartWidth .= "value" ]
--
borderInlineStartWidth :: MisoString -> Style
borderInlineStartWidth x = "border-inline-start-width" === x
-----------------------------------------------------------------------------
-- borderLeftColor
--
-- > styles_ $ M.fromList [ borderLeftColor .= "value" ]
--
borderLeftColor :: MisoString -> Style
borderLeftColor x = "border-left-color" === x
-----------------------------------------------------------------------------
-- borderLeft
--
-- > styles_ $ M.fromList [ borderLeft .= "value" ]
--
borderLeft :: MisoString -> Style
borderLeft x = "border-left" === x
-----------------------------------------------------------------------------
-- borderLeftStyle
--
-- > styles_ $ M.fromList [ borderLeftStyle .= "value" ]
--
borderLeftStyle :: MisoString -> Style
borderLeftStyle x = "border-left-style" === x
-----------------------------------------------------------------------------
-- borderLeftWidth
--
-- > styles_ $ M.fromList [ borderLeftWidth .= "value" ]
--
borderLeftWidth :: MisoString -> Style
borderLeftWidth x = "border-left-width" === x
-----------------------------------------------------------------------------
-- border
--
-- > styles_ $ M.fromList [ border .= "value" ]
--
border :: MisoString -> Style
border x = "border" === x
-----------------------------------------------------------------------------
-- borderRadius
--
-- > styles_ $ M.fromList [ borderRadius .= "value" ]
--
borderRadius :: MisoString -> Style
borderRadius x = "border-radius" === x
-----------------------------------------------------------------------------
-- borderRightColor
--
-- > styles_ $ M.fromList [ borderRightColor .= "value" ]
--
borderRightColor :: MisoString -> Style
borderRightColor x = "border-right-color" === x
-----------------------------------------------------------------------------
-- borderRight
--
-- > styles_ $ M.fromList [ borderRight .= "value" ]
--
borderRight :: MisoString -> Style
borderRight x = "border-right" === x
-----------------------------------------------------------------------------
-- borderRightStyle
--
-- > styles_ $ M.fromList [ borderRightStyle .= "value" ]
--
borderRightStyle :: MisoString -> Style
borderRightStyle x = "border-right-style" === x
-----------------------------------------------------------------------------
-- borderRightWidth
--
-- > styles_ $ M.fromList [ borderRightWidth .= "value" ]
--
borderRightWidth :: MisoString -> Style
borderRightWidth x = "border-right-width" === x
-----------------------------------------------------------------------------
-- borderStartEndRadius
--
-- > styles_ $ M.fromList [ borderStartEndRadius .= "value" ]
--
borderStartEndRadius :: MisoString -> Style
borderStartEndRadius x = "border-start-end-radius" === x
-----------------------------------------------------------------------------
-- borderStartStartRadius
--
-- > styles_ $ M.fromList [ borderStartStartRadius .= "value" ]
--
borderStartStartRadius :: MisoString -> Style
borderStartStartRadius x = "border-start-start-radius" === x
-----------------------------------------------------------------------------
-- borderStyle
--
-- > styles_ $ M.fromList [ borderStyle .= "value" ]
--
borderStyle :: MisoString -> Style
borderStyle x = "border-style" === x
-----------------------------------------------------------------------------
-- borderTopColor
--
-- > styles_ $ M.fromList [ borderTopColor .= "value" ]
--
borderTopColor :: MisoString -> Style
borderTopColor x = "border-top-color" === x
-----------------------------------------------------------------------------
-- borderTopLeftRadius
--
-- > styles_ $ M.fromList [ borderTopLeftRadius .= "value" ]
--
borderTopLeftRadius :: MisoString -> Style
borderTopLeftRadius x = "border-top-left-radius" === x
-----------------------------------------------------------------------------
-- borderTop
--
-- > styles_ $ M.fromList [ borderTop .= "value" ]
--
borderTop :: MisoString -> Style
borderTop x = "border-top" === x
-----------------------------------------------------------------------------
-- borderTopRightRadius
--
-- > styles_ $ M.fromList [ borderTopRightRadius .= "value" ]
--
borderTopRightRadius :: MisoString -> Style
borderTopRightRadius x = "border-top-right-radius" === x
-----------------------------------------------------------------------------
-- borderTopStyle
--
-- > styles_ $ M.fromList [ borderTopStyle .= "value" ]
--
borderTopStyle :: MisoString -> Style
borderTopStyle x = "border-top-style" === x
-----------------------------------------------------------------------------
-- borderTopWidth
--
-- > styles_ $ M.fromList [ borderTopWidth .= "value" ]
--
borderTopWidth :: MisoString -> Style
borderTopWidth x = "border-top-width" === x
-----------------------------------------------------------------------------
-- borderWidth
--
-- > styles_ $ M.fromList [ borderWidth .= "value" ]
--
borderWidth :: MisoString -> Style
borderWidth x = "border-width" === x
-----------------------------------------------------------------------------
-- bottom
--
-- > styles_ $ M.fromList [ bottom .= "value" ]
--
bottom :: MisoString -> Style
bottom x = "bottom" === x
-----------------------------------------------------------------------------
-- boxShadow
--
-- > styles_ $ M.fromList [ boxShadow .= "value" ]
--
boxShadow :: MisoString -> Style
boxShadow x = "box-shadow" === x
-----------------------------------------------------------------------------
-- boxSizing
--
-- > styles_ $ M.fromList [ boxSizing .= "value" ]
--
boxSizing :: MisoString -> Style
boxSizing x = "box-sizing" === x
-----------------------------------------------------------------------------
-- clipPath
--
-- > styles_ $ M.fromList [ clipPath .= "value" ]
--
clipPath :: MisoString -> Style
clipPath x = "clip-path" === x
-----------------------------------------------------------------------------
-- color
--
-- > styles_ $ M.fromList [ color .= "value" ]
--
color :: MisoString -> Style
color x = "color" === x
-----------------------------------------------------------------------------
-- columnGap
--
-- > styles_ $ M.fromList [ columnGap .= "value" ]
--
columnGap :: MisoString -> Style
columnGap x = "column-gap" === x
-----------------------------------------------------------------------------
-- cssVariable
--
-- > styles_ $ M.fromList [ cssVariable .= "value" ]
--
cssVariable :: MisoString -> Style
cssVariable x = "css-variable" === x
-----------------------------------------------------------------------------
-- direction
--
-- > styles_ $ M.fromList [ direction .= "value" ]
--
direction :: MisoString -> Style
direction x = "direction" === x
-----------------------------------------------------------------------------
-- display
--
-- > styles_ $ M.fromList [ display .= "value" ]
--
display :: MisoString -> Style
display x = "display" === x
-----------------------------------------------------------------------------
-- filter
--
-- > styles_ $ M.fromList [ filter .= "value" ]
--
filter :: MisoString -> Style
filter x = "filter" === x
-----------------------------------------------------------------------------
-- flexBasis
--
-- > styles_ $ M.fromList [ flexBasis .= "value" ]
--
flexBasis :: MisoString -> Style
flexBasis x = "flex-basis" === x
-----------------------------------------------------------------------------
-- flexDirection
--
-- > styles_ $ M.fromList [ flexDirection .= "value" ]
--
flexDirection :: MisoString -> Style
flexDirection x = "flex-direction" === x
-----------------------------------------------------------------------------
-- flexFlow
--
-- > styles_ $ M.fromList [ flexFlow .= "value" ]
--
flexFlow :: MisoString -> Style
flexFlow x = "flex-flow" === x
-----------------------------------------------------------------------------
-- flexGrow
--
-- > styles_ $ M.fromList [ flexGrow .= "value" ]
--
flexGrow :: MisoString -> Style
flexGrow x = "flex-grow" === x
-----------------------------------------------------------------------------
-- flex
--
-- > styles_ $ M.fromList [ flex .= "value" ]
--
flex :: MisoString -> Style
flex x = "flex" === x
-----------------------------------------------------------------------------
-- flexShrink
--
-- > styles_ $ M.fromList [ flexShrink .= "value" ]
--
flexShrink :: MisoString -> Style
flexShrink x = "flex-shrink" === x
-----------------------------------------------------------------------------
-- flexWrap
--
-- > styles_ $ M.fromList [ flexWrap .= "value" ]
--
flexWrap :: MisoString -> Style
flexWrap x = "flex-wrap" === x
-----------------------------------------------------------------------------
-- fontFamily
--
-- > styles_ $ M.fromList [ fontFamily .= "value" ]
--
fontFamily :: MisoString -> Style
fontFamily x = "font-family" === x
-----------------------------------------------------------------------------
-- fontSize
--
-- > styles_ $ M.fromList [ fontSize .= "value" ]
--
fontSize :: MisoString -> Style
fontSize x = "font-size" === x
-----------------------------------------------------------------------------
-- fontStyle
--
-- > styles_ $ M.fromList [ fontStyle .= "value" ]
--
fontStyle :: MisoString -> Style
fontStyle x = "font-style" === x
-----------------------------------------------------------------------------
-- fontWeight
--
-- > styles_ $ M.fromList [ fontWeight .= "value" ]
--
fontWeight :: MisoString -> Style
fontWeight x = "font-weight" === x
-----------------------------------------------------------------------------
-- gap
--
-- > styles_ $ M.fromList [ gap .= "value" ]
--
gap :: MisoString -> Style
gap x = "gap" === x
-----------------------------------------------------------------------------
-- gridAutoColumns
--
-- > styles_ $ M.fromList [ gridAutoColumns .= "value" ]
--
gridAutoColumns :: MisoString -> Style
gridAutoColumns x = "grid-auto-columns" === x
-----------------------------------------------------------------------------
-- gridAutoFlow
--
-- > styles_ $ M.fromList [ gridAutoFlow .= "value" ]
--
gridAutoFlow :: MisoString -> Style
gridAutoFlow x = "grid-auto-flow" === x
-----------------------------------------------------------------------------
-- gridAutoRows
--
-- > styles_ $ M.fromList [ gridAutoRows .= "value" ]
--
gridAutoRows :: MisoString -> Style
gridAutoRows x = "grid-auto-rows" === x
-----------------------------------------------------------------------------
-- gridColumnEnd
--
-- > styles_ $ M.fromList [ gridColumnEnd .= "value" ]
--
gridColumnEnd :: MisoString -> Style
gridColumnEnd x = "grid-column-end" === x
-----------------------------------------------------------------------------
-- gridColumnSpan
--
-- > styles_ $ M.fromList [ gridColumnSpan .= "value" ]
--
gridColumnSpan :: MisoString -> Style
gridColumnSpan x = "grid-column-span" === x
-----------------------------------------------------------------------------
-- gridColumnStart
--
-- > styles_ $ M.fromList [ gridColumnStart .= "value" ]
--
gridColumnStart :: MisoString -> Style
gridColumnStart x = "grid-column-start" === x
-----------------------------------------------------------------------------
-- gridRowEnd
--
-- > styles_ $ M.fromList [ gridRowEnd .= "value" ]
--
gridRowEnd :: MisoString -> Style
gridRowEnd x = "grid-row-end" === x
-----------------------------------------------------------------------------
-- gridRowSpan
--
-- > styles_ $ M.fromList [ gridRowSpan .= "value" ]
--
gridRowSpan :: MisoString -> Style
gridRowSpan x = "grid-row-span" === x
-----------------------------------------------------------------------------
-- gridRowStart
--
-- > styles_ $ M.fromList [ gridRowStart .= "value" ]
--
gridRowStart :: MisoString -> Style
gridRowStart x = "grid-row-start" === x
-----------------------------------------------------------------------------
-- gridTemplateColumns
--
-- > styles_ $ M.fromList [ gridTemplateColumns .= "value" ]
--
gridTemplateColumns :: MisoString -> Style
gridTemplateColumns x = "grid-template-columns" === x
-----------------------------------------------------------------------------
-- gridTemplateRows
--
-- > styles_ $ M.fromList [ gridTemplateRows .= "value" ]
--
gridTemplateRows :: MisoString -> Style
gridTemplateRows x = "grid-template-rows" === x
-----------------------------------------------------------------------------
-- height
--
-- > styles_ $ M.fromList [ height .= "value" ]
--
height :: MisoString -> Style
height x = "height" === x
-----------------------------------------------------------------------------
-- imageRendering
--
-- > styles_ $ M.fromList [ imageRendering .= "value" ]
--
imageRendering :: MisoString -> Style
imageRendering x = "image-rendering" === x
-----------------------------------------------------------------------------
-- insetInlineEnd
--
-- > styles_ $ M.fromList [ insetInlineEnd .= "value" ]
--
insetInlineEnd :: MisoString -> Style
insetInlineEnd x = "inset-inline-end" === x
-----------------------------------------------------------------------------
-- insetInlineStart
--
-- > styles_ $ M.fromList [ insetInlineStart .= "value" ]
--
insetInlineStart :: MisoString -> Style
insetInlineStart x = "inset-inline-start" === x
-----------------------------------------------------------------------------
-- justifyContent
--
-- > styles_ $ M.fromList [ justifyContent .= "value" ]
--
justifyContent :: MisoString -> Style
justifyContent x = "justify-content" === x
-----------------------------------------------------------------------------
-- justifyItems
--
-- > styles_ $ M.fromList [ justifyItems .= "value" ]
--
justifyItems :: MisoString -> Style
justifyItems x = "justify-items" === x
-----------------------------------------------------------------------------
-- justifySelf
--
-- > styles_ $ M.fromList [ justifySelf .= "value" ]
--
justifySelf :: MisoString -> Style
justifySelf x = "justify-self" === x
-----------------------------------------------------------------------------
-- left
--
-- > styles_ $ M.fromList [ left .= "value" ]
--
left :: MisoString -> Style
left x = "left" === x
-----------------------------------------------------------------------------
-- letterSpacing
--
-- > styles_ $ M.fromList [ letterSpacing .= "value" ]
--
letterSpacing :: MisoString -> Style
letterSpacing x = "letter-spacing" === x
-----------------------------------------------------------------------------
-- linearCrossGravity
--
-- > styles_ $ M.fromList [ linearCrossGravity .= "value" ]
--
linearCrossGravity :: MisoString -> Style
linearCrossGravity x = "linear-cross-gravity" === x
-----------------------------------------------------------------------------
-- linearDirection
--
-- > styles_ $ M.fromList [ linearDirection .= "value" ]
--
linearDirection :: MisoString -> Style
linearDirection x = "linear-direction" === x
-----------------------------------------------------------------------------
-- linearGravity
--
-- > styles_ $ M.fromList [ linearGravity .= "value" ]
--
linearGravity :: MisoString -> Style
linearGravity x = "linear-gravity" === x
-----------------------------------------------------------------------------
-- linearLayoutGravity
--
-- > styles_ $ M.fromList [ linearLayoutGravity .= "value" ]
--
linearLayoutGravity :: MisoString -> Style
linearLayoutGravity x = "linear-layout-gravity" === x
-----------------------------------------------------------------------------
-- linearWeight
--
-- > styles_ $ M.fromList [ linearWeight .= "value" ]
--
linearWeight :: MisoString -> Style
linearWeight x = "linear-weight" === x
-----------------------------------------------------------------------------
-- linearWeightSum
--
-- > styles_ $ M.fromList [ linearWeightSum .= "value" ]
--
linearWeightSum :: MisoString -> Style
linearWeightSum x = "linear-weight-sum" === x
-----------------------------------------------------------------------------
-- lineHeight
--
-- > styles_ $ M.fromList [ lineHeight .= "value" ]
--
lineHeight :: MisoString -> Style
lineHeight x = "line-height" === x
-----------------------------------------------------------------------------
-- marginBottom
--
-- > styles_ $ M.fromList [ marginBottom .= "value" ]
--
marginBottom :: MisoString -> Style
marginBottom x = "margin-bottom" === x
-----------------------------------------------------------------------------
-- marginInlineEnd
--
-- > styles_ $ M.fromList [ marginInlineEnd .= "value" ]
--
marginInlineEnd :: MisoString -> Style
marginInlineEnd x = "margin-inline-end" === x
-----------------------------------------------------------------------------
-- marginInlineStart
--
-- > styles_ $ M.fromList [ marginInlineStart .= "value" ]
--
marginInlineStart :: MisoString -> Style
marginInlineStart x = "margin-inline-start" === x
-----------------------------------------------------------------------------
-- marginLeft
--
-- > styles_ $ M.fromList [ marginLeft .= "value" ]
--
marginLeft :: MisoString -> Style
marginLeft x = "margin-left" === x
-----------------------------------------------------------------------------
-- margin
--
-- > styles_ $ M.fromList [ margin .= "value" ]
--
margin :: MisoString -> Style
margin x = "margin" === x
-----------------------------------------------------------------------------
-- marginRight
--
-- > styles_ $ M.fromList [ marginRight .= "value" ]
--
marginRight :: MisoString -> Style
marginRight x = "margin-right" === x
-----------------------------------------------------------------------------
-- marginTop
--
-- > styles_ $ M.fromList [ marginTop .= "value" ]
--
marginTop :: MisoString -> Style
marginTop x = "margin-top" === x
-----------------------------------------------------------------------------
-- maskImage
--
-- > styles_ $ M.fromList [ maskImage .= "value" ]
--
maskImage :: MisoString -> Style
maskImage x = "mask-image" === x
-----------------------------------------------------------------------------
-- mask
--
-- > styles_ $ M.fromList [ mask .= "value" ]
--
mask :: MisoString -> Style
mask x = "mask" === x
-----------------------------------------------------------------------------
-- maxHeight
--
-- > styles_ $ M.fromList [ maxHeight .= "value" ]
--
maxHeight :: MisoString -> Style
maxHeight x = "max-height" === x
-----------------------------------------------------------------------------
-- maxWidth
--
-- > styles_ $ M.fromList [ maxWidth .= "value" ]
--
maxWidth :: MisoString -> Style
maxWidth x = "max-width" === x
-----------------------------------------------------------------------------
-- minHeight
--
-- > styles_ $ M.fromList [ minHeight .= "value" ]
--
minHeight :: MisoString -> Style
minHeight x = "min-height" === x
-----------------------------------------------------------------------------
-- minWidth
--
-- > styles_ $ M.fromList [ minWidth .= "value" ]
--
minWidth :: MisoString -> Style
minWidth x = "min-width" === x
-----------------------------------------------------------------------------
-- opacity
--
-- > styles_ $ M.fromList [ opacity .= "value" ]
--
opacity :: MisoString -> Style
opacity x = "opacity" === x
-----------------------------------------------------------------------------
-- order
--
-- > styles_ $ M.fromList [ order .= "value" ]
--
order :: MisoString -> Style
order x = "order" === x
-----------------------------------------------------------------------------
-- overflow
--
-- > styles_ $ M.fromList [ overflow .= "value" ]
--
overflow :: MisoString -> Style
overflow x = "overflow" === x
-----------------------------------------------------------------------------
-- overflowX
--
-- > styles_ $ M.fromList [ overflowX .= "value" ]
--
overflowX :: MisoString -> Style
overflowX x = "overflow-x" === x
-----------------------------------------------------------------------------
-- overflowY
--
-- > styles_ $ M.fromList [ overflowY .= "value" ]
--
overflowY :: MisoString -> Style
overflowY x = "overflow-y" === x
-----------------------------------------------------------------------------
-- paddingBottom
--
-- > styles_ $ M.fromList [ paddingBottom .= "value" ]
--
paddingBottom :: MisoString -> Style
paddingBottom x = "padding-bottom" === x
-----------------------------------------------------------------------------
-- paddingInlineEnd
--
-- > styles_ $ M.fromList [ paddingInlineEnd .= "value" ]
--
paddingInlineEnd :: MisoString -> Style
paddingInlineEnd x = "padding-inline-end" === x
-----------------------------------------------------------------------------
-- paddingInlineStart
--
-- > styles_ $ M.fromList [ paddingInlineStart .= "value" ]
--
paddingInlineStart :: MisoString -> Style
paddingInlineStart x = "padding-inline-start" === x
-----------------------------------------------------------------------------
-- paddingLeft
--
-- > styles_ $ M.fromList [ paddingLeft .= "value" ]
--
paddingLeft :: MisoString -> Style
paddingLeft x = "padding-left" === x
-----------------------------------------------------------------------------
-- padding
--
-- > styles_ $ M.fromList [ padding .= "value" ]
--
padding :: MisoString -> Style
padding x = "padding" === x
-----------------------------------------------------------------------------
-- paddingRight
--
-- > styles_ $ M.fromList [ paddingRight .= "value" ]
--
paddingRight :: MisoString -> Style
paddingRight x = "padding-right" === x
-----------------------------------------------------------------------------
-- paddingTop
--
-- > styles_ $ M.fromList [ paddingTop .= "value" ]
--
paddingTop :: MisoString -> Style
paddingTop x = "padding-top" === x
-----------------------------------------------------------------------------
-- perspective
--
-- > styles_ $ M.fromList [ perspective .= "value" ]
--
perspective :: MisoString -> Style
perspective x = "perspective" === x
-----------------------------------------------------------------------------
-- position
--
-- > styles_ $ M.fromList [ position .= "value" ]
--
position :: MisoString -> Style
position x = "position" === x
-----------------------------------------------------------------------------
-- relativeAlignBottom
--
-- > styles_ $ M.fromList [ relativeAlignBottom .= "value" ]
--
relativeAlignBottom :: MisoString -> Style
relativeAlignBottom x = "relative-align-bottom" === x
-----------------------------------------------------------------------------
-- relativeAlignInlineEnd
--
-- > styles_ $ M.fromList [ relativeAlignInlineEnd .= "value" ]
--
relativeAlignInlineEnd :: MisoString -> Style
relativeAlignInlineEnd x = "relative-align-inline-end" === x
-----------------------------------------------------------------------------
-- relativeAlignInlineStart
--
-- > styles_ $ M.fromList [ relativeAlignInlineStart .= "value" ]
--
relativeAlignInlineStart :: MisoString -> Style
relativeAlignInlineStart x = "relative-align-inline-start" === x
-----------------------------------------------------------------------------
-- relativeAlignLeft
--
-- > styles_ $ M.fromList [ relativeAlignLeft .= "value" ]
--
relativeAlignLeft :: MisoString -> Style
relativeAlignLeft x = "relative-align-left" === x
-----------------------------------------------------------------------------
-- relativeAlignRight
--
-- > styles_ $ M.fromList [ relativeAlignRight .= "value" ]
--
relativeAlignRight :: MisoString -> Style
relativeAlignRight x = "relative-align-right" === x
-----------------------------------------------------------------------------
-- relativeAlignTop
--
-- > styles_ $ M.fromList [ relativeAlignTop .= "value" ]
--
relativeAlignTop :: MisoString -> Style
relativeAlignTop x = "relative-align-top" === x
-----------------------------------------------------------------------------
-- relativeBottomOf
--
-- > styles_ $ M.fromList [ relativeBottomOf .= "value" ]
--
relativeBottomOf :: MisoString -> Style
relativeBottomOf x = "relative-bottom-of" === x
-----------------------------------------------------------------------------
-- relativeCenter
--
-- > styles_ $ M.fromList [ relativeCenter .= "value" ]
--
relativeCenter :: MisoString -> Style
relativeCenter x = "relative-center" === x
-----------------------------------------------------------------------------
-- relativeId
--
-- > styles_ $ M.fromList [ relativeId .= "value" ]
--
relativeId :: MisoString -> Style
relativeId x = "relative-id" === x
-----------------------------------------------------------------------------
-- relativeInlineEndOf
--
-- > styles_ $ M.fromList [ relativeInlineEndOf .= "value" ]
--
relativeInlineEndOf :: MisoString -> Style
relativeInlineEndOf x = "relative-inline-end-of" === x
-----------------------------------------------------------------------------
-- relativeInlineStartOf
--
-- > styles_ $ M.fromList [ relativeInlineStartOf .= "value" ]
--
relativeInlineStartOf :: MisoString -> Style
relativeInlineStartOf x = "relative-inline-start-of" === x
-----------------------------------------------------------------------------
-- relativeLayoutOnce
--
-- > styles_ $ M.fromList [ relativeLayoutOnce .= "value" ]
--
relativeLayoutOnce :: MisoString -> Style
relativeLayoutOnce x = "relative-layout-once" === x
-----------------------------------------------------------------------------
-- relativeLeftOf
--
-- > styles_ $ M.fromList [ relativeLeftOf .= "value" ]
--
relativeLeftOf :: MisoString -> Style
relativeLeftOf x = "relative-left-of" === x
-----------------------------------------------------------------------------
-- relativeRightOf
--
-- > styles_ $ M.fromList [ relativeRightOf .= "value" ]
--
relativeRightOf :: MisoString -> Style
relativeRightOf x = "relative-right-of" === x
-----------------------------------------------------------------------------
-- relativeTopOf
--
-- > styles_ $ M.fromList [ relativeTopOf .= "value" ]
--
relativeTopOf :: MisoString -> Style
relativeTopOf x = "relative-top-of" === x
-----------------------------------------------------------------------------
-- right
--
-- > styles_ $ M.fromList [ right .= "value" ]
--
right :: MisoString -> Style
right x = "right" === x
-----------------------------------------------------------------------------
-- rowGap
--
-- > styles_ $ M.fromList [ rowGap .= "value" ]
--
rowGap :: MisoString -> Style
rowGap x = "row-gap" === x
-----------------------------------------------------------------------------
-- textAlign
--
-- > styles_ $ M.fromList [ textAlign .= "value" ]
--
textAlign :: MisoString -> Style
textAlign x = "text-align" === x
-----------------------------------------------------------------------------
-- textDecoration
--
-- > styles_ $ M.fromList [ textDecoration .= "value" ]
--
textDecoration :: MisoString -> Style
textDecoration x = "text-decoration" === x
-----------------------------------------------------------------------------
-- textIndent
--
-- > styles_ $ M.fromList [ textIndent .= "value" ]
--
textIndent :: MisoString -> Style
textIndent x = "text-indent" === x
-----------------------------------------------------------------------------
-- textOverflow
--
-- > styles_ $ M.fromList [ textOverflow .= "value" ]
--
textOverflow :: MisoString -> Style
textOverflow x = "text-overflow" === x
-----------------------------------------------------------------------------
-- textShadow
--
-- > styles_ $ M.fromList [ textShadow .= "value" ]
--
textShadow :: MisoString -> Style
textShadow x = "text-shadow" === x
-----------------------------------------------------------------------------
-- textStrokeColor
--
-- > styles_ $ M.fromList [ textStrokeColor .= "value" ]
--
textStrokeColor :: MisoString -> Style
textStrokeColor x = "text-stroke-color" === x
-----------------------------------------------------------------------------
-- textStroke
--
-- > styles_ $ M.fromList [ textStroke .= "value" ]
--
textStroke :: MisoString -> Style
textStroke x = "text-stroke" === x
-----------------------------------------------------------------------------
-- textStrokeWidth
--
-- > styles_ $ M.fromList [ textStrokeWidth .= "value" ]
--
textStrokeWidth :: MisoString -> Style
textStrokeWidth x = "text-stroke-width" === x
-----------------------------------------------------------------------------
-- top
--
-- > styles_ $ M.fromList [ top .= "value" ]
--
top :: MisoString -> Style
top x = "top" === x
-----------------------------------------------------------------------------
-- transform
--
-- > styles_ $ M.fromList [ transform .= "value" ]
--
transform :: MisoString -> Style
transform x = "transform" === x
-----------------------------------------------------------------------------
-- transformOrigin
--
-- > styles_ $ M.fromList [ transformOrigin .= "value" ]
--
transformOrigin :: MisoString -> Style
transformOrigin x = "transform-origin" === x
-----------------------------------------------------------------------------
-- transitionDelay
--
-- > styles_ $ M.fromList [ transitionDelay .= "value" ]
--
transitionDelay :: MisoString -> Style
transitionDelay x = "transition-delay" === x
-----------------------------------------------------------------------------
-- transitionDuration
--
-- > styles_ $ M.fromList [ transitionDuration .= "value" ]
--
transitionDuration :: MisoString -> Style
transitionDuration x = "transition-duration" === x
-----------------------------------------------------------------------------
-- transition
--
-- > styles_ $ M.fromList [ transition .= "value" ]
--
transition :: MisoString -> Style
transition x = "transition" === x
-----------------------------------------------------------------------------
-- transitionProperty
--
-- > styles_ $ M.fromList [ transitionProperty .= "value" ]
--
transitionProperty :: MisoString -> Style
transitionProperty x = "transition-property" === x
-----------------------------------------------------------------------------
-- transitionTimingFunction
--
-- > styles_ $ M.fromList [ transitionTimingFunction .= "value" ]
--
transitionTimingFunction :: MisoString -> Style
transitionTimingFunction x = "transition-timing-function" === x
-----------------------------------------------------------------------------
-- verticalAlign
--
-- > styles_ $ M.fromList [ verticalAlign .= "value" ]
--
verticalAlign :: MisoString -> Style
verticalAlign x = "vertical-align" === x
-----------------------------------------------------------------------------
-- visibility
--
-- > styles_ $ M.fromList [ visibility .= "value" ]
--
visibility :: MisoString -> Style
visibility x = "visibility" === x
-----------------------------------------------------------------------------
-- whiteSpace
--
-- > styles_ $ M.fromList [ whiteSpace .= "value" ]
--
whiteSpace :: MisoString -> Style
whiteSpace x = "white-space" === x
-----------------------------------------------------------------------------
-- width
--
-- > styles_ $ M.fromList [ width .= "value" ]
--
width :: MisoString -> Style
width x = "width" === x
-----------------------------------------------------------------------------
-- wordBreak
--
-- > styles_ $ M.fromList [ wordBreak .= "value" ]
--
wordBreak :: MisoString -> Style
wordBreak x = "word-break" === x
-----------------------------------------------------------------------------
-- xAutoFontSize
--
-- > styles_ $ M.fromList [ xAutoFontSize .= "value" ]
--
xAutoFontSize :: MisoString -> Style
xAutoFontSize x = "-x-auto-font-size" === x
-----------------------------------------------------------------------------
-- xAutoFontSizePresetSizes
--
-- > styles_ $ M.fromList [ xAutoFontSizePresetSizes .= "value" ]
--
xAutoFontSizePresetSizes :: MisoString -> Style
xAutoFontSizePresetSizes x = "-x-auto-font-size-preset-sizes" === x
-----------------------------------------------------------------------------
-- xHandleColor
--
-- > styles_ $ M.fromList [ xHandleColor .= "value" ]
--
xHandleColor :: MisoString -> Style
xHandleColor x = "-x-handle-color" === x
-----------------------------------------------------------------------------
-- xHandleSize
--
-- > styles_ $ M.fromList [ xHandleSize .= "value" ]
--
xHandleSize :: MisoString -> Style
xHandleSize x = "-x-handle-size" === x
-----------------------------------------------------------------------------
-- zIndex
--
-- > styles_ $ M.fromList [ zIndex .= "value" ]
--
zIndex :: MisoString -> Style
zIndex x = "z-index" === x
-----------------------------------------------------------------------------
