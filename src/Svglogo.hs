{-# LANGUAGE OverloadedStrings #-}

module Svglogo where

import Text.Blaze.Svg11 as S
import Text.Blaze.Svg11.Attributes as A

svglogo = S.docTypeSvg ! A.viewbox "0 0 465.89471 465.89471" $ do
  S.circle ! A.id_ "circle"
           ! A.cx "233"
           ! A.cy "233"
           ! A.r "226"
           ! A.fill "none"
           ! A.strokeWidth "10"
  S.path ! A.id_ "body1"
         ! A.fill "none"
         ! A.strokeWidth "20"
         ! A.d "M 47.276126,346.87203 C 106.30443,287.84374 255.91,283.80794 256.39227,345.10224 c 0.48031,61.04412 -144.74446,66.14146 -209.116144,1.76979 0,-121.74214 137.728274,-208.37569 303.000734,-183.4217"
  S.path ! A.id_ "body2"
         ! A.fill "none"
         ! A.strokeWidth "20"
         ! A.d "m 400.95526,179.4456 c 91.43028,35.80764 10.47281,78.64939 -43.90816,120.56398 -36.55661,28.17631 -29.92198,49.76327 16.06177,106.05151"
  S.circle ! A.id_ "eye"
           ! A.fill "none"
           ! A.cx "376.77936"
           ! A.cy "169.57791"
           ! A.r "21.000422"
           ! A.strokeWidth "20"

