{-# OPTIONS  -Wall             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module CoAuthors where

import Text.HTML.Combinators
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

type URL = Maybe Text

urlToText :: Text -> URL -> Text
urlToText name = maybe name (flip a_ name)

data Person =
  Person { firstname :: Text
         , surname   :: Text
         , website   :: URL }

personToText :: Person -> Text
personToText Person{..} = urlToText name website
  where name = (T.head firstname `T.cons` ". " `T.append` surname)

cmcbride :: Person
cmcbride =
  Person { firstname = "Conor"
         , surname   = "McBride"
         , website   = Just "http://strictlypositive.org/" }

gallais :: Person
gallais =
  Person { firstname = "Guillaume"
         , surname   = "Allais"
         , website   = Nothing }

jnagele :: Person
jnagele =
  Person { firstname = "Julian"
         , surname   = "Nagele"
         , website   = Nothing }

pboutillier :: Person
pboutillier =
  Person { firstname = "Pierre"
         , surname   = "Boutillier"
         , website   = Just "http://www.pps.univ-paris-diderot.fr/~pboutill/index.en.html" }

rthiemann :: Person
rthiemann =
  Person { firstname = "René"
         , surname   = "Thiemann"
         , website   = Just "http://cl-informatik.uibk.ac.at/users/thiemann/" }

ybertot :: Person
ybertot =
  Person { firstname = "Yves"
         , surname   = "Bertot"
         , website   = Just "http://www-sop.inria.fr/members/Yves.Bertot" }
