{-# OPTIONS  -Wall             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Text.HTML.Combinators where

import Control.Applicative
import Control.Monad.State as CMS
import Data.Monoid ((<>))
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

data HTMLState = HTMLState
  { nbSections   :: !Int
  , nbFootnotes  :: !Int
  , getFootnotes :: [Text]
  }

initHTMLState :: HTMLState
initHTMLState = HTMLState 0 1 []

newSection :: MonadState HTMLState m => m Int
newSection = do
  i <- CMS.gets nbSections
  CMS.modify $ \ st -> st { nbSections = i + 1 }
  return i

newFootnote :: MonadState HTMLState m => (Int -> Text) -> m Int
newFootnote k = do
  i <- CMS.gets nbFootnotes
  CMS.modify $ \ st -> st { nbFootnotes  = i + 1
                          , getFootnotes = k i : getFootnotes st }
  return i

sandwichedWith :: Text -> Text -> Text
str `sandwichedWith` bread = T.concat [ tag "" , str , tag "/" ]
  where tag elt = T.concat [ "<" , elt , bread , ">" ]

a_ :: Text -> Text -> Text
a_ href txt =
  T.concat [ "<a href=\"" , href , "\">"
           , txt
           , "</a>" ]

aWith_ :: Text -> Text -> Text -> Text
aWith_ name href txt =
  T.concat [ "<a id=\"" , name , "\""
           , " href=\"" , href , "\">"
           , txt
           , "</a>" ]

aClass_ :: Text -> Text -> Text -> Text
aClass_ name href txt =
  T.concat [ "<a class=\"" , name , "\""
           , " href=\"" , href , "\">"
           , txt
           , "</a>" ]

anchor :: Text -> Text
anchor name = T.concat [ "<a name=\"" , name , "\" />" ]

br_ :: Text
br_ = "<br />"

b_ :: Text -> Text
b_ str = str `sandwichedWith` "b"

i_ :: Text -> Text
i_ str = str `sandwichedWith` "i"

u_ :: Text -> Text
u_ str = str `sandwichedWith` "u"

tt_:: Text -> Text
tt_ str = str `sandwichedWith` "tt"

ul_ :: [Text] -> Text
ul_ = ulWith_ ""

ulWith_ :: Text -> [Text] -> Text
ulWith_ sep strs =
  T.intercalate (sep `T.snoc` '\n') (fmap li_ strs) `sandwichedWith` "ul"

li_ :: Text -> Text
li_ str = str `sandwichedWith` "li"

img_ :: Text -> Text
img_ str = T.concat [ "<img src=\"" , str , "\" />" ]

center_ :: Text -> Text
center_ str = str `sandwichedWith` "center"

footnote_ :: MonadState HTMLState m => Text -> m Text
footnote_ str = do
  i <- newFootnote $ \ i ->
    let ftWrapper = p_ " class=\"footnote\""
        ftNumber  = div_ " class=\"footnote-number\""
        ftBody    = div_ " class=\"footnote-body\""
        footnote  = ftNumber (ref i "bot" "top") `T.append` ftBody str
    in ftWrapper footnote
  return $ ref i "top" "bot"

  where

    ref :: Int -> Text -> Text -> Text
    ref num here there =
      let num'    = T.pack (show num)
          anchorH = T.concat [ "ref" , here , num' ]
          anchorT = T.concat [ "#ref" , there, num' ]
      in aWith_ anchorH anchorT $ T.cons '[' $ num' `T.snoc` ']'

block_ :: Text -> Text -> Text -> Text
block_ name tag str =
  T.concat [ "<" , name , tag , ">"
           , str
           , "</" , name , ">" ]

div_ :: Text -> Text -> Text
div_ = block_ "div"

p_ :: Text -> Text -> Text
p_ = block_ "p"

span_ :: Text -> Text -> Text
span_ = block_ "span"

h_ :: Int -> Text -> Text
h_ n title = T.concat [ "<" , tag , title , "</" , tag ]
  where tag = T.concat [ "h" , T.pack $ show n , ">" ]

ah'_ :: Int -> Text -> Text -> Text
ah'_ n i title =
  let link = aClass_ "sectionref" ("#" <> i) "#" in
  T.concat
         [ anchor i
         , "\n"
         , h_ n (link <> " " <> title)
         ]

ah_ :: (Functor m, MonadState HTMLState m) => Int -> Maybe Text -> Text -> m Text
ah_ n anc title = do
  i <- case anc of
         Nothing -> T.append "section" . T.pack . show <$> newSection
         Just x  -> return x
  pure $ ah'_ n i title
