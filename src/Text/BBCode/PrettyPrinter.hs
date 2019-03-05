{-# OPTIONS  -Wall             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Text.BBCode.PrettyPrinter where

import Text.BBCode.Parser
import Text.HTML.Combinators

import Control.Applicative
import Control.Monad.State as CMS

import Hakyll.Core.Item
import Hakyll.Core.Compiler

import qualified Data.Char as C
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy          as T
import qualified Data.Text.Lazy.Encoding as TE


instance PrettyBBCode Tag where
  prettyBBCode t = return $ case t of
    NULL       -> ""
    ID name    -> " id="    `T.append` outName name
    CLASS name -> " class=" `T.append` outName name

class PrettyBBCode a where
  prettyBBCode :: (Functor m, Applicative m, MonadState HTMLState m) => a -> m Text

instance PrettyBBCode Content where
  prettyBBCode c = case c of
    BR            -> return br_
    RAW txt       -> return txt
    I txts        -> i_ <$> prettyBBCode txts
    B txts        -> b_ <$> prettyBBCode txts
    U txts        -> u_ <$> prettyBBCode txts
    TT txts       -> tt_ <$> prettyBBCode txts
    URL href txts -> a_ (outUrl href) <$> prettyBBCode txts
    IMG txt       -> return $ img_ $ outUrl txt
    SPAN tag txts -> span_ <$> prettyBBCode tag <*> prettyBBCode txts
    CENTER txts   -> center_ <$> prettyBBCode txts
    FOOTNOTE txts -> footnote_ =<< prettyBBCode txts

instance PrettyBBCode a => PrettyBBCode [a] where
  prettyBBCode = fmap T.concat . mapM prettyBBCode

instance PrettyBBCode Structure where
  prettyBBCode s = case s of
    TXT cs       -> prettyBBCode cs
    DIV tag txts -> div_ <$> prettyBBCode tag <*> prettyBBCode txts
    P tag txts   -> p_ <$> prettyBBCode tag <*> prettyBBCode txts
    UL lis       -> ul_ <$> mapM prettyBBCode lis
    H int txts   -> ah_ int (title txts) =<< prettyBBCode txts
    where
      title :: [Content] -> Maybe Text
      title txts = case txts of
        [RAW t] -> pure $ T.concat $ T.words $ toTitle t
        _ -> Nothing

      capitalize :: Text -> Text
      capitalize t = case T.uncons t of
        Nothing      -> ""
        Just (c, t') -> T.cons (C.toUpper c) t'

      toTitle :: Text -> Text
      toTitle t = T.unwords $ fmap capitalize $ T.words t

ppBBCode :: (Functor m, Applicative m, MonadState HTMLState m) => [Structure] -> m Text
ppBBCode strs = T.intercalate "\n" <$> mapM prettyBBCode strs

bbcodeCompiler :: Compiler (Item Text)
bbcodeCompiler = do
  txt <- itemBody <$> getResourceLBS
  let bbc       = parseBBCode $ TE.decodeUtf8 txt
  let html      = either (return . T.pack . show) ppBBCode bbc
  let (res, ft) = runState html initHTMLState
  makeItem $ T.concat $ res :
    if null (getFootnotes ft) then []
    else
      let footnotes = T.concat $ reverse $ getFootnotes ft
      in [ ah'_ 3 "Footnotes" "Footnotes", div_ " class=\"footnotes\"" footnotes ]
