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

import Data.List
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy          as T
import qualified Data.Text.Lazy.Encoding as TE

ppTag :: Tag -> Text
ppTag NULL         = ""
ppTag (ID    name) = " id="    `T.append` outName name
ppTag (CLASS name) = " class=" `T.append` outName name

ppList :: (Functor m, MonadState Footnotes m) => (a -> m Text) -> [a] -> m Text
ppList pp = fmap T.concat . mapM pp

ppContent :: (Functor m, MonadState Footnotes m) => Content -> m Text
ppContent BR              = return br_
ppContent (RAW      txt)  = return txt
ppContent (I        txts) = liftM i_                  $ ppList ppContent txts
ppContent (B        txts) = liftM b_                  $ ppList ppContent txts
ppContent (U        txts) = liftM u_                  $ ppList ppContent txts
ppContent (TT       txts) = liftM tt_                 $ ppList ppContent txts
ppContent (URL href txts) = liftM (a_ $ outUrl href)  $ ppList ppContent txts
ppContent (IMG      txt)  = return $ img_             $ outUrl txt
ppContent (SPAN tag txts) = liftM (span_ $ ppTag tag) $ ppList ppContent txts
ppContent (CENTER   txts) = liftM center_             $ ppList ppContent txts
ppContent (FOOTNOTE txts) = footnote_               =<< ppList ppContent txts

ppStructure :: (Functor m, MonadState Footnotes m) => Structure -> m Text
ppStructure (TXT cs)       = ppList ppContent cs
ppStructure (DIV tag txts) = liftM (div_ $ ppTag tag) $ ppList ppStructure txts
ppStructure (H   int txts) = liftM (h_   int)         $ ppList ppContent   txts
ppStructure (P   tag txts) = liftM (p_   $ ppTag tag) $ ppList ppStructure txts
ppStructure (UL      lis)  = liftM ul_                $ mapM (ppList ppContent) lis

ppBBCode :: (Functor m, MonadState Footnotes m) => [Structure] -> m Text
ppBBCode = fmap (T.intercalate "\n") . mapM ppStructure

bbcodeCompiler :: Compiler (Item Text)
bbcodeCompiler = do
  txt <- itemBody <$> getResourceLBS
  let bbc       = parseBBCode $ TE.decodeUtf8 txt
  let html      = either (return . T.pack . show) ppBBCode bbc
  let (res, ft) = runState html (Footnotes 1 [])
  makeItem $ T.concat $ res :
    if null (list ft) then []
    else h_ 3 "Footnotes" : intersperse br_ (reverse $ list ft)
