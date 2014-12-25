{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid
import Publications
import Text.BBCode.PrettyPrinter
import Hakyll

import qualified Data.Text.Lazy as T

main :: IO ()
main = hakyll $ do
    match "img/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "pdf/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*.ttf" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*.css" $ do
        route   idRoute
        compile compressCssCompiler

    match "blog/*.txt" $ do
        route $ setExtension "html"
        compile $ fmap (fmap T.unpack) bbcodeCompiler
          >>= loadAndApplyTemplate "templates/default.html" defaultContext

    create ["publis.html"] $ do
        route idRoute
        compile $
          return Item { itemIdentifier = fromFilePath "publis.html"
                      , itemBody       = T.unpack publications }
          >>= loadAndApplyTemplate "templates/default.html" defaultContext
          >>= relativizeUrls

    match (fromList ["index.html" , "contact.html"]) $ do
        route idRoute
        compile $
          getResourceBody
          >>= loadAndApplyTemplate "templates/default.html" defaultContext
          >>= relativizeUrls

    match "templates/*" $ compile templateCompiler
