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

aabel :: Person
aabel = Person
  { firstname = "Andreas"
  , surname   = "Abel"
  , website   = Just "http://www.cse.chalmers.se/~abela/"
  }

ahameer :: Person
ahameer = Person
  { firstname = "Aliya"
  , surname   = "Hameer"
  , website   = Nothing
  }

bpientka :: Person
bpientka = Person
  { firstname = "Brigitte"
  , surname   = "Pientka"
  , website   = Just "https://www.cs.mcgill.ca/~bpientka/"
  }

amomigliano :: Person
amomigliano = Person
  { firstname = "Alberto"
  , surname   = "Momigliano"
  , website   = Just "http://momigliano.di.unimi.it/"
  }

sschafer :: Person
sschafer = Person
  { firstname = "Steven"
  , surname   = "Schäfer"
  , website   = Just "https://www.ps.uni-saarland.de/~schaefer/"
  }

kstark :: Person
kstark = Person
  { firstname = "Kathrin"
  , surname   = "Stark"
  , website   = Just "https://www.ps.uni-saarland.de/~kstark/"
  }

ratkey :: Person
ratkey =
  Person { firstname = "Robert"
         , surname   = "Atkey"
         , website   = Just "http://bentnib.org/"
         }

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

jmchapman :: Person
jmchapman =
  Person { firstname = "James"
         , surname   = "Chapman"
         , website   = Just "http://cs.ioc.ee/~james/" }

jmckinna :: Person
jmckinna =
  Person { firstname = "James"
         , surname   = "McKinna"
         , website   = Nothing }

jnagele :: Person
jnagele =
  Person { firstname = "Julian"
         , surname   = "Nagele"
         , website   = Just "http://cl-informatik.uibk.ac.at/users/csag8264/"
         }

pboutillier :: Person
pboutillier =
  Person { firstname = "Pierre"
         , surname   = "Boutillier"
         , website   = Just "https://github.com/pirbo"
         }

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
