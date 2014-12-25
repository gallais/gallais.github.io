{-# OPTIONS  -Wall             #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.HTML.Combinators where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy          as T
import qualified Data.Text.Lazy.Encoding as TE

sandwichedWith :: Text -> Text -> Text
str `sandwichedWith` bread = T.concat [ tag "" , str , tag "/" ]
  where tag elt = T.concat [ "<" , elt , bread , ">" ]

a_ :: Text -> Text -> Text
a_ href name =
  T.concat [ "<a href=\"" , href , "\">"
           , name
           , "</a>" ]

b_ :: Text -> Text
b_ str = str `sandwichedWith` "b"

i_ :: Text -> Text
i_ str = str `sandwichedWith` "i"

u_ :: Text -> Text
u_ str = str `sandwichedWith` "u"

tt_:: Text -> Text
tt_ str = str `sandwichedWith` "tt"

ul_ :: [Text] -> Text
ul_ strs = T.intercalate "\n" (fmap li_ strs) `sandwichedWith` "ul"

li_ :: Text -> Text
li_ str = str `sandwichedWith` "li"

img_ :: Text -> Text
img_ str = str `sandwichedWith` "img"

center_ :: Text -> Text
center_ str = str `sandwichedWith` "center"

footnote_ :: Text -> Text
footnote_ str = str `sandwichedWith` "footnote"

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
