{-# OPTIONS  -Wall             #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Publications where

import Text.HTML.Combinators

import Data.Text.Lazy (Text, intercalate)
import qualified Data.Text.Lazy as T

import CoAuthors

data Venue =
  Venue { name :: Text
        , www  :: URL }

venueToText :: Venue -> Text
venueToText v = urlToText (name v) (www v)

data Kind = Pdf | Slides | Github | Agda | Blog

kindToText :: Kind -> Text
kindToText Pdf    = "pdf"
kindToText Slides = "slides"
kindToText Github = "github"
kindToText Agda   = "agda"
kindToText Blog   = "blog"

data Resource =
  Resource { kind :: Kind
           , link :: Text }

pdf :: Text -> Resource
pdf url = Resource { kind = Pdf, link = url }

slides :: Text -> Resource
slides url = Resource { kind = Slides, link = url }

github :: Text -> Resource
github url = Resource { kind = Github, link = url }

agda :: Text -> Resource
agda url = Resource { kind = Agda, link = url }

blog :: Text -> Resource
blog url = Resource { kind = Blog, link = url }

resourceToText :: Resource -> Text
resourceToText res = urlToText (kindToText $ kind res) (Just $ link res)

resourcesToText :: [Resource] -> Text
resourcesToText [] = ""
resourcesToText rs = span_ " class=\"docs\"" $ "[ " `T.append` docs `T.append` " ]"
  where docs = intercalate " | " $ fmap resourceToText rs

data Date =
  Date { day   :: Maybe Int
       , month :: Maybe Int
       , year  :: Int }

dateToText :: Date -> Text
dateToText d = go (day d) `T.append` go (month d) `T.append` T.pack (show (year d))
  where
    go Nothing  = ""
    go (Just i) = (if i < 10 then "0" else "") `T.append` T.pack (show i) `T.append` " "

yearOnly :: Int -> Date
yearOnly y =
  Date { day   = Nothing
       , month = Nothing
       , year  = y }

data Publi =
  Publi { authors   :: [Person]
        , title     :: Text
        , date      :: Date
        , venue     :: Venue
        , resources :: [Resource] }

publiToText :: Publi -> Text
publiToText Publi{..} =
  T.concat
    [ b_ title , docs , br_ , bys
    , span_ " class=\"docs\"" $ T.concat [ conf , ", " , time ] ]
  where
    docs = resourcesToText resources
    bys  = intercalate ", " $ fmap personToText authors
    conf = venueToText venue
    time = dateToText date

data Sort =
    Journal
  | Conference
  | Workshop
  | TechReport
  | Talk

sortToText :: Sort -> Text
sortToText Journal    = "Journal papers"
sortToText Conference = "Conference papers"
sortToText Workshop   = "Workshops"
sortToText TechReport = "Technical reports"
sortToText Talk       = "Talks"

data Publis =
  Publis { sort   :: Sort
         , publis :: [Publi] }

publisToText :: Publis -> Text
publisToText Publis{..} = T.concat [ h_ 3 h3 , ulWith_ "<hr />" lis ]
  where
    h3  = sortToText sort
    lis = fmap publiToText publis

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
  [ Publi { authors   = [rthiemann, gallais, jnagele]
          , title     = "On the Formalization of Termination Techniques Based on Multiset Orderings"
          , date      = yearOnly 2012
          , venue     = Venue { name = "RTA", www = Just "http://rta2012.trs.cm.is.nagoya-u.ac.jp/" }
          , resources = [ pdf "pdf/rta2012.pdf" ]
          }
  ]

journals :: [Publi]
journals =
  [ Publi { authors   = [ybertot, gallais]
          , title     = "Views of PI: Definition and computation"
          , date      = Date Nothing (Just 10) 2014
          , venue     = Venue { name = "Journal of Formalized Reasoning", www = Just "http://jfr.unibo.it/" }
          , resources = [ pdf "http://jfr.unibo.it/article/view/4343" ]
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
  [ Publis { sort = Journal   , publis = journals }
  , Publis { sort = Conference, publis = conferences }
  , Publis { sort = Workshop  , publis = workshops   }
  , Publis { sort = Talk      , publis = talks       }
  , Publis { sort = TechReport, publis = reports     }
  ]

publications :: Text
publications = T.concat $ fmap publisToText allPublis



--
-- 	array ( authors => 'G. Allais',
-- 	        title   => 'Coq with Power Series',
-- 	        year    => '2011',
-- 		conf    => 'THedu',
-- 		confwww => 'http://www.uc.pt/en/congressos/thedu/thedu11',
-- 		pdf     => 'thedu11.pdf',
-- 		slides  => 'thedu11_slides.pdf')
--   )),
