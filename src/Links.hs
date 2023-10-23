{-# OPTIONS  -Wall             #-}
{-# LANGUAGE OverloadedStrings #-}

module Links where

import Text.HTML.Combinators
import Data.Text as T

links :: Text
links =
  let a_img_  = \ (a, i) -> a_ ("http://" `T.append` a) $ img_ $ T.concat [ "/img/" , i , ".png" ]
      pclass_ = p_ " class=\"\"" . T.concat . fmap a_img_
  in T.concat $
  [ h_ 3 "People"
  , pclass_ linksPeople
  , h_ 3 "Forums"
  , pclass_ linksForum
  , h_ 3 "Media & Advocacy"
  , pclass_ linksMedia
  , h_ 3 "Credits"
  , p_ "" "The « constructivism » on this website was snapped in London. \
          \ It is part of the Tate Modern Gallery's timeline and refers  \
          \ originally to the Russian-born artistic philosophy."
  ]

linksPeople :: [(Text, Text)]
linksPeople =
  [ ("gaupy.org", "gaupy")
  , ("www.lirmm.fr/~bonamy/", "marthe")
  , ("www.loria.fr/~hferee/", "hugo")
  , ("madiot.org", "jm")
  , ("www.pps.univ-paris-diderot.fr/~pedrot/", "pmp")
  ]

linksForum :: [(Text, Text)]
linksForum =
  [ ("cstheory.stackexchange.com", "cst")
  , ("reddit.com/r/dependent_types", "reddit")
  ]


linksMedia :: [(Text, Text)]
linksMedia =
  [ ("lmsi.net", "lmsi")
  , ("le-tigre.net", "tigre")
  , ("acrimed.org", "acrimed")
  --, ("www.medialens.org", "medialens")
  , ("mediapart.fr", "mediapart")
  , ("laquadrature.net", "quadrature")
  ]
