{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid
import Control.Monad
import Publications (publications)
import BlogList
import Links
import Text.BBCode.PrettyPrinter
import Text.HTML.Combinators
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
    (nullRSS "gallais' blog" ("http://" <> domain <> "/blog/"))
      { rssChannel = (nullChannel "gallais' blog" ("http://" <> domain <> "/blog/"))
      { rssItems   = items } }


main :: IO ()
main = hakyll $ do

    let rssIcon = span_ " style=\"float:right\"" $ a_ "/rss.xml" $ img_ "/img/rss.png"
    forM blogPosts $ \ post ->
      create [ fromFilePath $ "blog/" ++ source post ++ ".txt" ] $ do
        let title = constField "title" (T.unpack (name post))
        route   $ setExtension "html"
        compile $ fmap (fmap $ T.unpack . ((rssIcon <> h_ 1 (name post)) <>)) bbcodeCompiler
          >>= saveSnapshot "content"
          >>= loadAndApplyTemplate "templates/default.html" (title <> defaultContext)
          >>= relativizeUrls

    forM allTags $ \ key ->
      create [ fromFilePath $ "blog/blog." ++ T.unpack key ++ ".html" ] $ do
        route idRoute
        compile $
          makeItem (T.unpack $ blogIndex $ Just key)
          >>= loadAndApplyTemplate "templates/default.html" defaultContext
          >>= relativizeUrls

    create ["blog/index.html"] $ do
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

    create ["links.html"] $ do
        route idRoute
        compile $
          makeItem (T.unpack links)
          >>= loadAndApplyTemplate "templates/default.html" defaultContext
          >>= relativizeUrls

    match "templates/*" $ compile templateCompiler
