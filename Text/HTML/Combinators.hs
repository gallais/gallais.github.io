{-# OPTIONS  -Wall             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Text.HTML.Combinators where

import Control.Applicative
import Control.Monad.State as CMS
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

data Footnotes =
  Footnotes { next :: Int
            , list :: [Text] }

push :: MonadState Footnotes m => Text -> m ()
push str = CMS.modify $ \ (Footnotes n fs) -> Footnotes (n + 1) (str : fs)

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
img_ str = str `sandwichedWith` "img"

center_ :: Text -> Text
center_ str = str `sandwichedWith` "center"


footnote_ :: (Functor m, MonadState Footnotes m) => Text -> m Text
footnote_ str = do
  i <- T.pack . show <$> CMS.gets next
  push   $ ref i "bot" "top" `T.append` T.cons ' ' str
  return $ ref i "top" "bot"
  where
    ref num here there =
      let anchorH = T.concat [ "ref" , here , num ]
          anchorT = T.concat [ "#ref" , there, num ]
      in aWith_ anchorH anchorT $ T.cons '[' $ num `T.snoc` ']'

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
