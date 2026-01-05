{-# OPTIONS  -Wall             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module CoAuthors where

import Text.HTML.Combinators
import Data.Text (Text)
import qualified Data.Text as T

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

amomigliano :: Person
amomigliano = Person
  { firstname = "Alberto"
  , surname   = "Momigliano"
  , website   = Just "http://momigliano.di.unimi.it/"
  }

arice :: Person
arice = Person
  { firstname = "Alex"
  , surname = "Rice"
  , website = Nothing
  }

bpientka :: Person
bpientka = Person
  { firstname = "Brigitte"
  , surname   = "Pientka"
  , website   = Just "https://www.cs.mcgill.ca/~bpientka/"
  }

cmcbride :: Person
cmcbride = Person
  { firstname = "Conor"
  , surname   = "McBride"
  , website   = Just "http://strictlypositive.org/"
  }

croy :: Person
croy = Person
  { firstname = "Craig"
  , surname = "Roy"
  , website = Nothing
  }

dkidney :: Person
dkidney = Person
  { firstname = "Donnacha Oisín"
  , surname = "Kidney"
  , website = Nothing
  }

ebrady :: Person
ebrady = Person
  { firstname = "Edwin"
  , surname   = "Brady"
  , website   = Just "https://www.type-driven.org.uk/edwinb/"
  }

fnf :: Person
fnf = Person
  { firstname = "Fredrik"
  , surname = "Nordvall Forsberg"
  , website = Just "https://personal.cis.strath.ac.uk/fredrik.nordvall-forsberg/"
  }

gallais :: Person
gallais = Person
  { firstname = "Guillaume"
  , surname   = "Allais"
  , website   = Nothing
  }

gnakov :: Person
gnakov = Person
  { firstname = "Georgi"
  , surname = "Nakov"
  , website = Nothing
  }

jcarette :: Person
jcarette = Person
  { firstname = "Jacques"
  , surname = "Carette"
  , website = Just "http://www.cas.mcmaster.ca/~carette"
  }

jfdm :: Person
jfdm = Person
  { firstname = "Jan"
  , surname = "de Muijnck-Hughes"
  , website = Just "https://jfdm.github.io/"
  }

jhu :: Person
jhu = Person
  { firstname = "Jason Z. S."
  , surname = "Hu"
  , website = Nothing
  }

jmchapman :: Person
jmchapman = Person
  { firstname = "James"
  , surname = "Chapman"
  , website = Nothing
  }

jmckinna :: Person
jmckinna = Person
  { firstname = "James"
  , surname   = "McKinna"
  , website   = Nothing
  }

jnagele :: Person
jnagele = Person
  { firstname = "Julian"
  , surname   = "Nagele"
  , website   = Just "https://jnagele.net/"
  }

jwood :: Person
jwood = Person
  { firstname = "James"
  , surname = "Wood"
  , website = Nothing
  }

jyallop :: Person
jyallop = Person
  { firstname = "Jeremy"
  , surname   = "Yallop"
  , website   = Just "https://www.cl.cam.ac.uk/~jdy22/"
  }

kstark :: Person
kstark = Person
  { firstname = "Kathrin"
  , surname   = "Stark"
  , website   = Just "https://www.k-stark.de/"
  }

lxia :: Person
lxia = Person
  { firstname = "Li-yao"
  , surname = "Xia"
  , website = Nothing
  }

maltenmuller :: Person
maltenmuller = Person
  { firstname = "Malin"
  , surname = "Altenmüller"
  , website = Just "https://maltenmuller.github.io/"
  }

mdaggitt :: Person
mdaggitt = Person
  { firstname = "Matthew"
  , surname = "Daggit"
  , website = Nothing
  }

ncorbyn :: Person
ncorbyn = Person
  { firstname = "Nathan"
  , surname = "Corbyn"
  , website = Just "https://nathancorbyn.com/"
  }

nvdoorn :: Person
nvdoorn = Person
  { firstname = "Nathan"
  , surname = "van Doorn"
  , website = Nothing
  }

okammar :: Person
okammar = Person
  { firstname = "Ohad"
  , surname   = "Kammar"
  , website   = Just "http://denotational.co.uk/"
  }

pboutillier :: Person
pboutillier = Person
  { firstname = "Pierre"
  , surname   = "Boutillier"
  , website   = Just "https://github.com/pirbo"
  }

ratkey :: Person
ratkey = Person
  { firstname = "Robert"
  , surname   = "Atkey"
  , website   = Just "http://bentnib.org/"
  }

rmullanix :: Person
rmullanix = Person
  { firstname = "Reed"
  , surname = "Mullanix"
  , website = Nothing
  }

rthiemann :: Person
rthiemann = Person
  { firstname = "René"
  , surname   = "Thiemann"
  , website   = Just "http://cl-informatik.uibk.ac.at/users/thiemann/"
  }

smechveliani :: Person
smechveliani = Person
  { firstname = "Sergei"
  , surname = "Meshveliani"
  , website = Nothing
  }

sschafer :: Person
sschafer = Person
  { firstname = "Steven"
  , surname   = "Schäfer"
  , website   = Just "https://www.ps.uni-saarland.de/~schaefer/"
  }

sstucki :: Person
sstucki = Person
  { firstname = "Sandro"
  , surname = "Stucki"
  , website = Nothing
  }

syou :: Person
syou = Person
  { firstname = "Shu-Hung"
  , surname = "You"
  , website = Nothing
  }

unorell :: Person
unorell = Person
  { firstname = "Ulf"
  , surname = "Norell"
  , website = Just "https://www.cse.chalmers.se/~ulfn/"
  }

wkokke :: Person
wkokke = Person
  { firstname = "Wen"
  , surname = "Kokke"
  , website = Nothing
  }

ybertot :: Person
ybertot = Person
  { firstname = "Yves"
  , surname   = "Bertot"
  , website   = Just "http://www-sop.inria.fr/members/Yves.Bertot"
  }
