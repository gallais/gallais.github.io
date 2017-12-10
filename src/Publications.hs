{-# OPTIONS  -Wall             #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Publications where

import Text.HTML.Combinators

import Data.Monoid
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
dateToText d = T.pack (show (year d)) <> go (month d) <> go (day d)
  where
    go Nothing  = ""
    go (Just i) = " " <> (if i < 10 then "0" else "") <> T.pack (show i)

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
  T.unlines
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
  | Draft

sortToText :: Sort -> Text
sortToText Journal    = "Journal papers"
sortToText Conference = "Conference papers"
sortToText Workshop   = "Workshops"
sortToText TechReport = "Technical reports"
sortToText Talk       = "Talks"
sortToText Draft      = "Drafts"

data Publis =
  Publis { sort   :: Sort
         , publis :: [Publi] }

publisToText :: Publis -> Text
publisToText Publis{..} = T.unlines [ h_ 3 h3 , ulWith_ "<hr />" lis ]
  where
    h3  = sortToText sort
    lis = fmap publiToText publis

workshops :: [Publi]
workshops =
  [
    Publi { authors   = [gallais]
          , title     = "agdarsec — Total Parser Combinators"
          , date      = Date Nothing (Just 1) 2018
          , venue     = Venue { name = "JFLA", www = Just "https://www.lri.fr/~sboldo/JFLA18/" }
          , resources = [ github "https://github.com/gallais/agdarsec"
                        , pdf "pdf/agdarsec18.pdf"
                        ]
          }
  , Publi { authors   = [gallais]
          , title     = "agdARGS - Declarative Hierarchical Command Line Interfaces"
          , date      = yearOnly 2017
          , venue     = Venue { name = "TTT", www = Just "http://popl17.sigplan.org/track/TTT-2017" }
          , resources = [ pdf "pdf/TTT-2017.pdf"
                        , slides "pdf/TTT-2017_slides.pdf"
                        , github "https://github.com/gallais/agdARGS" ]
          }
  , Publi { authors   = [gallais, pboutillier, cmcbride]
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
          , resources = [ pdf "pdf/itp11.pdf", slides "pdf/itp11_slides.pdf" ]
          }
  ]

conferences :: [Publi]
conferences =
  [
    Publi { authors   = [gallais, jmchapman, cmcbride, jmckinna]
          , title     = "Type-and-Scope Safe Programs and their Proofs"
          , date      = yearOnly 2017
          , venue     = Venue { name = "CPP", www = Just "http://cpp2017.mpi-sws.org/" }
          , resources = [ pdf "pdf/cpp2017.pdf"
                        , slides "pdf/cpp2017_slides.pdf"
                        , github "https://github.com/gallais/type-scope-semantics" ]
          }

  , Publi { authors   = [rthiemann, gallais, jnagele]
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
          , title     = "agdarsec - Total Parser Combinators in Agda"
          , date      = Date (Just 04) (Just 04) 2017
          , venue     = Venue { name = "Brouwer Seminar"
                              , www  = Just "https://www.cs.ru.nl/is/foundations/seminar/"
                              }
          , resources = [ github "https://github.com/gallais/agdarsec"
                        , slides "pdf/brouwer17.pdf"
                        ]
          }
  , Publi { authors   = [gallais, jmchapman, cmcbride]
          , title     = "Type and Scope Preserving Semantics"
          , date      = Date (Just 21) (Just 10) 2015
          , venue     = Venue { name = "Scottish PL Seminar"
                              , www = Just "http://simonjf.com/spls-oct2015/" }
          , resources = [ github "https://github.com/gallais/type-scope-semantics"
                        , slides "https://github.com/gallais/type-scope-semantics/blob/master/slides.pdf" ]
          }
  , Publi { authors   = [gallais]
          , title     = "agdARGS - Command Line Arguments, Options and Flags"
          , date      = Date (Just 18) (Just 3) 2015
          , venue     = Venue { name = "Idris Developers Meeting"
                              , www  = Just "https://github.com/idris-lang/Idris-dev/wiki/Idris-Developers-Meeting,-March-2015"
                              }
          , resources = [ github "https://github.com/gallais/agdARGS"
                        , slides "https://github.com/gallais/agdARGS/blob/master/doc/2015-03-18-IIM.pdf" ]
          }
  , Publi { authors   = [gallais]
          , title     = "Resource Aware Contexts and Proof Search for IMLL"
          , date      = Date (Just 30) (Just 6) 2014
          , venue     = Venue { name = "PL Interest Seminar - University of Edinburgh", www = Nothing }
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
                        , blog "blog/glueing-terms-models.html" ]
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

drafts :: [Publi]
drafts =
  [

    Publi { authors   = [gallais]
          , title     = "Typing with Leftovers - A Mechanization of Intuitionistic Linear Logic"
          , date      = Date Nothing Nothing 2017
          , venue     = Venue { name = "Submitted to TYPES", www = Nothing }
          , resources = [ github "https://github.com/gallais/typing-with-leftovers"
                        , pdf "pdf/leftovers17.pdf" ]
          }
  , Publi { authors   = [gallais, ratkey, jmchapman, cmcbride, jmckinna]
          , title     = "A Scope Safe Universe of Syntaxes with Binding, Their Semantics and Proofs"
          , date      = Date Nothing (Just 4) 2017
          , venue     = Venue { name = "Submitted to FSCD", www = Nothing }
          , resources = [ github "https://github.com/gallais/generic-syntax"
                        , pdf "pdf/draft_fscd17.pdf"
                        ]
          }
  , Publi { authors   = [gallais, cmcbride]
          , title     = "Certified Proof Search for Intuitionistic Linear Logic"
          , date      = Date Nothing (Just 2) 2015
          , venue     = Venue { name = "Submitted to TLCA", www = Nothing }
          , resources = [ github "https://github.com/gallais/proof-search-ILLWiL"
                        , pdf "http://gallais.github.io/proof-search-ILLWiL/" ]
          }
  ]

allPublis :: [Publis]
allPublis =
  [ Publis { sort = Journal   , publis = journals }
  , Publis { sort = Conference, publis = conferences }
  , Publis { sort = Workshop  , publis = workshops   }
  , Publis { sort = Talk      , publis = talks       }
  , Publis { sort = Draft     , publis = drafts      }
  , Publis { sort = TechReport, publis = reports     }
  ]

publications :: Text
publications = T.unlines $ fmap publisToText allPublis



--
-- 	array ( authors => 'G. Allais',
-- 	        title   => 'Coq with Power Series',
-- 	        year    => '2011',
-- 		conf    => 'THedu',
-- 		confwww => 'http://www.uc.pt/en/congressos/thedu/thedu11',
-- 		pdf     => 'thedu11.pdf',
-- 		slides  => 'thedu11_slides.pdf')
--   )),
