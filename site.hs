{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid
import Control.Monad
import Publications
import BlogList
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

    forM blogPosts $ \ post ->
      create [ fromFilePath $ "blog/" ++ source post ++ ".txt" ] $ do
        route   $ setExtension "html"
        compile $ fmap (fmap T.unpack) bbcodeCompiler
          >>= loadAndApplyTemplate "templates/default.html" defaultContext
          >>= relativizeUrls

    forM allTags $ \ key ->
      create [ fromFilePath $ "blog." ++ T.unpack key ++ ".html" ] $ do
        route idRoute
        compile $
          makeItem (T.unpack $ blogIndex $ Just key)
          >>= loadAndApplyTemplate "templates/default.html" defaultContext
          >>= relativizeUrls

    create ["blog.html"] $ do
        route idRoute
        compile $
          makeItem (T.unpack $ blogIndex Nothing)
          >>= loadAndApplyTemplate "templates/default.html" defaultContext
          >>= relativizeUrls

    create ["publis.html"] $ do
        route idRoute
        compile $
          makeItem (T.unpack publications)
          >>= loadAndApplyTemplate "templates/default.html" defaultContext
          >>= relativizeUrls

    match (fromList ["index.html" , "contact.html"]) $ do
        route idRoute
        compile $
          getResourceBody
          >>= loadAndApplyTemplate "templates/default.html" defaultContext
          >>= relativizeUrls

    match "templates/*" $ compile templateCompiler
