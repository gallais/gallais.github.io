{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Publications where

import Data.List
import Text.Whiskers
import CoAuthors

data Venue =
  Venue { name :: String
        , www  :: URL }

venueToString :: Venue -> String
venueToString v = urlToString (name v) (www v)

data Kind = Pdf | Slides | Github | Agda | Blog

kindToString :: Kind -> String
kindToString Pdf    = "pdf"
kindToString Slides = "slides"
kindToString Github = "github"
kindToString Agda   = "agda"
kindToString Blog   = "blog"

data Resource =
  Resource { kind :: Kind
           , link :: String }

pdf :: String -> Resource
pdf url = Resource { kind = Pdf, link = url }

slides :: String -> Resource
slides url = Resource { kind = Slides, link = url }

github :: String -> Resource
github url = Resource { kind = Github, link = url }

agda :: String -> Resource
agda url = Resource { kind = Agda, link = url }

blog :: String -> Resource
blog url = Resource { kind = Blog, link = url }

resourceToString :: Resource -> String
resourceToString res = urlToString (kindToString $ kind res) (Just $ link res)

resourcesToString :: [Resource] -> String
resourcesToString [] = ""
resourcesToString rs = [whiskers| <span class="docs">[{{ docs }}]</span> |]
  where docs = intercalate "|" $ fmap resourceToString rs

data Date =
  Date { day   :: Maybe Int
       , month :: Maybe Int
       , year  :: Int }

dateToString :: Date -> String
dateToString d = go (day d) ++ go (month d) ++ show (year d)
  where
    go Nothing  = ""
    go (Just i) = (if i < 10 then "0" else "") ++ show i ++ " "

yearOnly :: Int -> Date
yearOnly y =
  Date { day   = Nothing
       , month = Nothing
       , year  = y }

data Publi =
  Publi { authors   :: [Person]
        , title     :: String
        , date      :: Date
        , venue     :: Venue
        , resources :: [Resource] }

publiToText :: Publi -> String
publiToText Publi{..} =
  [whiskers|
    <b>{{ title }}</b> {{ docs }}
    <br /> {{ bys }} <span class="docs">{{ conf }}, {{ time }}</span>
  |]
  where
    docs = resourcesToString resources
    bys  = intercalate ", " $ fmap personToString authors
    conf = venueToString venue
    time = dateToString date

data Sort =
    Conference
  | Workshop
  | TechReport
  | Talk

sortToString :: Sort -> String
sortToString Conference = "Conference papers"
sortToString Workshop   = "Workshop papers"
sortToString TechReport = "Technical reports"
sortToString Talk       = "Talks"

data Publis =
  Publis { sort   :: Sort
         , publis :: [Publi] }

publisToText :: Publis -> String
publisToText Publis{..} = [whiskers| <h3>{{ h3 }}</h3> <ul><li> {{ lis }} </li></ul> |]
  where
    h3  = sortToString sort
    lis = intercalate "</li><hr />\n<li>" $ fmap publiToText publis

workshops :: [Publi]
workshops =
  [ Publi { authors   = [gallais, pboutillier, cmcbride]
          , title     = "New Equations for Neutral Terms: A Sound and Complete Decision Procedure, Formalized"
          , date      = yearOnly 2013
          , venue     = Venue { name = "DTP", www = Just "http://www.seas.upenn.edu/~sweirich/dtp13/" }
          , resources = [ pdf "pdf/icfp13.pdf" ]
          }
  , Publi { authors   = [gallais]
          , title     = "Using Reflection to Solve some Differential Equations"
          , date      = yearOnly 2011
          , venue     = Venue { name = "3rd Coq Workshop"
                              , www = Just "http://www.cs.ru.nl/~spitters/coqw.html" }
          , resources = [ pdf "pdf/itp11.pdf", slides "itp11_slides.pdf" ]
          }
  ]

conferences :: [Publi]
conferences =
  [ Publi { authors   = [gallais, rthiemann]
          , title     = "On the Formalization of Termination Techniques Based on Multiset Orderings"
          , date      = yearOnly 2012
          , venue     = Venue { name = "RTA", www = Just "http://rta2012.trs.cm.is.nagoya-u.ac.jp/" }
          , resources = [ pdf "pdf/rta2012.pdf" ]
          }
  ]

talks :: [Publi]
talks =
  [ Publi { authors   = [gallais]
          , title     = "Resource Aware Contexts and Proof Search for IMLL"
          , date      = Date (Just 30) (Just 6) 2014
          , venue     = Venue { name = "Pl Interest Semniare - University of Edinburgh", www = Nothing }
          , resources = [ github "https://github.com/gallais/potpourri/tree/master/agda/lps" ]
          }
  , Publi { authors   = [gallais]
          , title     = "Generic Generalised de Bruijn indices"
          , date      = Date (Just 17) (Just 2) 2014
          , venue     = Venue { name = "Gallium Seminar - INRIA Paris-Rocquencourt", www = Nothing }
          , resources = []
          }
  , Publi { authors   = [gallais]
          , title     = "Glueing Terms to Models: Variations on NbE"
          , date      = Date (Just 5) (Just 6) 2013
          , venue     = Venue { name = "MSP Away Day - University of Strathclyde", www = Nothing }
          , resources = [ pdf "pdf/awayday13.pdf"
                        , blog "glueing-terms-model" ]
          }
  ]

reports :: [Publi]
reports =
  [ Publi { authors   = [gallais]
          , title     = "Forge Crowbars, Acquire Normal Forms"
          , date      = yearOnly 2013
          , venue     = Venue { name = "University of Strathclyde", www = Nothing }
          , resources = [ pdf "pdf/report2012.pdf"
                        , github "https://github.com/gallais/agda-nbe" ]
          }
  ]

allPublis :: [Publis]
allPublis =
  [ Publis { sort = Conference, publis = conferences }
  , Publis { sort = Workshop  , publis = workshops   }
  , Publis { sort = Talk      , publis = talks       }
  , Publis { sort = TechReport, publis = reports     }
  ]

publications :: String
publications = concatMap publisToText allPublis



--
-- 	array ( authors => 'G. Allais',
-- 	        title   => 'Coq with Power Series',
-- 	        year    => '2011',
-- 		conf    => 'THedu',
-- 		confwww => 'http://www.uc.pt/en/congressos/thedu/thedu11',
-- 		pdf     => 'thedu11.pdf',
-- 		slides  => 'thedu11_slides.pdf')
--   )),
