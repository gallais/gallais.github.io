{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid
import Control.Monad
import Publications
import BlogList
import Text.BBCode.PrettyPrinter
import Text.RSS.Syntax
import Text.Feed.Constructor
import Text.Feed.Export
import Text.XML.Light.Output
import Hakyll

import qualified Data.Text.Lazy as T

blogRSS :: Compiler RSS
blogRSS = do
  items <-
    forM blogPosts $ \ post ->
      loadSnapshotBody (fromFilePath $ "blog/" ++ source post ++ ".txt") "content"
      >>= return . postRSS post
  return $
    (nullRSS "gallais' blog" "http://blog.gallais.org")
      { rssChannel = (nullChannel "gallais' blog" "http://blog.gallais.org")
      { rssItems   = items } }


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
          >>= saveSnapshot "content"
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

    create ["rss.xml"] $ do
         route idRoute
         compile $ blogRSS >>= makeItem . showElement . xmlFeed . feedFromRSS

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
