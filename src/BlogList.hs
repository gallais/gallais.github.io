{-# OPTIONS  -Wall             #-}
{-# LANGUAGE OverloadedStrings #-}

module BlogList where

import Data.Time
import System.Locale
import qualified Data.List as L
import Data.Text.Lazy as T

import Text.RSS.Syntax
import Text.HTML.Combinators

data BlogPost =
  BlogPost { name      :: Text
           , source :: FilePath
           , pubDate   :: UTCTime
           , keywords  :: [Text] }

mkDay :: Day -> UTCTime
mkDay d = UTCTime { utctDay = d, utctDayTime = 60 * (37 + 60 * 13) }


blogPosts :: [BlogPost]
blogPosts =
  [
--    BlogPost { name     = "Resource Aware Contexts and Proof Search for IMLL"
--             , source   = "linear-proof-search"
--             , pubDate  = mkDay $ fromGregorian 2014 12 26
--             , keywords = [linearLogic, tactics, reflection]
--             }
    BlogPost { name     = "Cyclic Lists, Purely"
             , source   = "cyclic-list-purely"
             , pubDate  = mkDay $ fromGregorian 2014 08 12
             , keywords = [datatype, functionalProgramming, haskell, phantomTypes, typeSafety]
             }
--  , BlogPost { name     = "The Simple Elegance of Hereditary Substitution"
--             , source   = "elegance-hereditary-substitution"
--             , pubDate  = mkDay $ fromGregorian 2014 07 30
--             , keywords = [normalization, dependentTypes, haskell]
--             }
  , BlogPost { name     = "Non-regular Parameters are OK"
             , source   = "non-regular-parameters"
             , pubDate  = mkDay $ fromGregorian 2014 07 17
             , keywords = [datatype, soundness, dependentTypes, agda, coq]
             }
  , BlogPost { name     = "Lazy Weakening and Equality Test"
             , source   = "lazy-lambda"
             , pubDate  = mkDay $ fromGregorian 2014 04 28
             , keywords = [binders, lazyness, λcalculus]
             }
  , BlogPost { name     = "Dimension-Aware Computations"
             , source   = "dimension-aware-computations"
             , pubDate  = mkDay $ fromGregorian 2013 11 02
             , keywords = [dimensions, dependentTypes, agda, typeSafety, physics]
             }
  , BlogPost { name     = "Glueing terms to models"
             , source   = "glueing-terms-models"
             , pubDate  = mkDay $ fromGregorian 2013 04 30
             , keywords = [normalization, agda, λcalculus, glueing]
             }
  , BlogPost { name     = "A universe for syntax with binding"
             , source   = "syntax-binding-run-omega"
             , pubDate  = mkDay $ fromGregorian 2013 04 11
             , keywords = [universe, agda, λcalculus, binders]
             }
  ]
  where
  -- maintaining a list of terms here to make sure that I reuse as many
  -- of the keywords as possible rather than introducing subtle variants.
    agda                  = "Agda"
    binders               = "binders"
    coq                   = "coq"
    datatype              = "datatype"
    dependentTypes        = "dependent types"
    dimensions            = "dimensions"
    functionalProgramming = "functional programming"
    glueing               = "glueing"
    haskell               = "haskell"
    λcalculus             = "lambda calculus"
    lazyness              = "lazyness"
--    linearLogic           = "linear logic"
    normalization         = "normalization"
    phantomTypes          = "phantom types"
    physics               = "physics"
--    reflection            = "reflection"
    soundness             = "soundness"
--    tactics               = "tactics"
    typeSafety            = "type safety"
    universe              = "universe"

allTags :: [Text]
allTags = L.nub $ L.concatMap keywords blogPosts

allTagsWithMultiplicity :: [(Text, Int)]
allTagsWithMultiplicity = flip fmap allTags $ \ k ->
  (k, L.length $ L.filter (k ==) $ L.concatMap keywords blogPosts)


blogIndex :: Maybe Text -> Text
blogIndex key = T.concat
  [ span_ " style=\"float:right\"" $ a_ "/rss.xml" $ img_ "/img/rss.png"
  , h_ 3 $ "Blog posts" `T.append` titleExt key
  , ul_  $ fmap post $ collectPosts key blogPosts
  , h_ 3 "Tags cloud"
  , T.intercalate " " multiplicities
  ]
  where
    multiplicities =
      fmap (\ (k, i) ->
        a_ (T.concat [ "blog." , k , ".html" ])
           (T.concat [ k , " (" , T.pack $ show i , ")" ]))
      $ L.filter ((> 1) . snd) allTagsWithMultiplicity

    titleExt Nothing  = ""
    titleExt (Just k) = T.concat [ " with tag \"", k, "\"" ]

    collectPosts Nothing  = id
    collectPosts (Just k) = L.filter (elem k . keywords)

    post :: BlogPost -> Text
    post bp =
      let date = formatTime defaultTimeLocale (iso8601DateFormat Nothing) in
      T.concat
          [ a_ (T.concat [ "blog/", T.pack $ source bp, ".html" ]) $ name bp
          , span_ " class=\"date\"" $ T.pack $ date $ pubDate bp ]

postRSS :: BlogPost -> String -> RSSItem
postRSS bp txt =
  let url = L.concat [ "http://www.gallais.org/blog/", source bp , ".html" ]
      css = "<head> \
            \<link rel=\"stylesheet\" type=\"text/css\" \
            \      href=\"http://www.gallais.org/css/main.css\" />\
            \</head>" in
  RSSItem { rssItemTitle       = Just $ T.unpack $ name bp
          , rssItemLink        = Just url
          , rssItemDescription = Just $ css ++ txt
          , rssItemAuthor      = Nothing
          , rssItemCategories  = []
          , rssItemComments    = Nothing
          , rssItemEnclosure   = Nothing
          , rssItemGuid        = Just $ RSSGuid (Just True) [] url
          , rssItemPubDate     = Just $ formatTime defaultTimeLocale rfc822DateFormat $ pubDate bp
          , rssItemSource      = Nothing
          , rssItemAttrs       = []
          , rssItemOther       = []
          }
