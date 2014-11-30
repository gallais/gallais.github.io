{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module CoAuthors where

import Text.Whiskers

type URL = Maybe String

urlToString :: String -> URL -> String
urlToString name Nothing    = name
urlToString name (Just url) = [whiskers| <a href="{{ url }}">{{ name }}</a> |]

data Person =
  Person { firstname :: String
         , surname   :: String
         , website   :: URL }

personToString :: Person -> String
personToString Person{..} = urlToString name website
  where name = (head firstname : ". " ++ surname)

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

