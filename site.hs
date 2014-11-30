{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid
import Publications
import Hakyll

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

    match (fromList ["fun.html", "contact.html"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    create ["publis.html"] $ do
        route idRoute
        compile $
          return Item { itemIdentifier = fromFilePath "publis.html"
                      , itemBody       = publications }
          >>= loadAndApplyTemplate "templates/default.html" defaultContext
          >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $
          getResourceBody
          >>= loadAndApplyTemplate "templates/default.html" defaultContext
          >>= relativizeUrls

    match "templates/*" $ compile templateCompiler
