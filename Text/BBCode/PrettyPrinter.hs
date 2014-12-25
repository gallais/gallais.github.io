{-# OPTIONS  -Wall             #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.BBCode.PrettyPrinter where

import Text.BBCode.Parser
import Text.HTML.Combinators

import Control.Applicative
import Control.Monad.State

import Hakyll.Core.Item
import Hakyll.Core.Compiler

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy          as T
import qualified Data.Text.Lazy.Encoding as TE

ppTag :: Tag -> Text
ppTag NULL         = ""
ppTag (ID    name) = " id="    `T.append` outName name
ppTag (CLASS name) = " class=" `T.append` outName name

ppList :: (a -> Text) -> [a] -> Text
ppList pp = T.concat . fmap pp

ppContent :: Content -> Text
ppContent BR            = "<br />"
ppContent (RAW      txt)  = txt
ppContent (I        txts) = i_                $ ppList ppContent txts
ppContent (B        txts) = b_                $ ppList ppContent txts
ppContent (U        txts) = u_                $ ppList ppContent txts
ppContent (TT       txts) = tt_               $ ppList ppContent txts
ppContent (URL href txts) = a_ (outUrl href)  $ ppList ppContent txts
ppContent (IMG      txt)  = img_              $ outUrl txt
ppContent (SPAN tag txts) = span_ (ppTag tag) $ ppList ppContent txts
ppContent (CENTER   txts) = center_           $ ppList ppContent txts
ppContent (FOOTNOTE txts) = footnote_         $ ppList ppContent txts

ppStructure :: Structure -> Text
ppStructure (TXT cs)       = ppList ppContent cs
ppStructure (DIV tag txts) = div_ (ppTag tag) $ ppList ppStructure txts
ppStructure (H   int txts) = h_   int         $ ppList ppContent   txts
ppStructure (P   tag txts) = p_   (ppTag tag) $ ppList ppStructure txts
ppStructure (UL      lis)  = ul_              $ fmap (ppList ppContent) lis

ppBBCode :: [Structure] -> Text
ppBBCode = T.intercalate "\n" . fmap ppStructure

bbcodeCompiler :: Compiler (Item Text)
bbcodeCompiler = do
  txt <- itemBody <$> getResourceLBS
  let bbc = parseBBCode $ TE.decodeUtf8 txt
  makeItem $ either (const "") ppBBCode bbc
