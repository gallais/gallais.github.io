{-# OPTIONS  -Wall       #-}
{-# LANGUAGE FlexibleContexts #-}

module BBCodeParser where

import Control.Applicative hiding (many, (<|>))
import Text.Parsec

newtype Name = Name { outName :: String }
 deriving Show

newtype Url  = Url  { outUrl  :: String }
 deriving Show

data Tag = NULL | ID Name | CLASS Name
 deriving Show

data Structure =
    DIV Tag [Structure]
  | H   Int [Content]
  | P   Tag [Structure]
  | UL      [[Content]]
  | TXT     [Content]
 deriving Show

data Content =
    BR
  | I            [Content]
  | B            [Content]
  | U            [Content]
  | TT           [Content]
  | SPAN     Tag [Content]
  | CENTER       [Content]
  | FOOTNOTE     [Content]
  | URL      Url [Content]
  | IMG      Url
  | RAW     String
 deriving Show

url :: Stream s m Char => ParsecT s u m (Url, [Content])
url = do
  link <- between (char '=') (char ']') $ many1 (try $ noneOf "]")
  rest <- contents
  _    <- string "[/url]"
  return (Url link, rest)

name :: Stream s m Char => ParsecT s u m Name
name = Name <$> many alphaNum


withTags :: Stream s m Char => ParsecT s u m a -> ParsecT s u m (Tag, a)
withTags p = (,) <$> tag <*> p
  where
    tag :: Stream s m Char => ParsecT s u m Tag
    tag = choice
      [ ID         <$> between (string "id=") (char ']') name
      , CLASS      <$> between (string "=")   (char ']') name
      , const NULL <$> string "]"
      ]

structures :: Stream s m Char => ParsecT s u m [Structure]
structures = many1 structure

ul :: Stream s m Char => ParsecT s u m [[Content]]
ul =
  (do
    _      <- char '['
    switch <- choice $ fmap string [ "li" , "/ul" ]
    case switch of
      "li"  -> (:) <$> (finishWith "[/li]" contents) <*> ul
      "/ul" -> pure [] <* char ']'
  ) <|> newline *> fmap ([RAW "\n"]:) ul

structure :: Stream s m Char => ParsecT s u m Structure
structure =
  (do
    switch <- choice $ fmap (try . string . ('[':))
              [ "div" , "h" , "p" , "ul" ]
    case tail switch of
      "div" -> uncurry DIV <$> withTags structures <* string "[/div]"
      "h"   -> H           <$> (read . (: []) <$> digit) <*> finishWith "[/h]" contents
      "p"   -> uncurry P   <$> withTags structures <* string "[/p]"
      "ul"  -> UL          <$> (char ']' *> ul)
  ) <|> TXT <$> contents

contents :: Stream s m Char => ParsecT s u m [Content]
contents = many1 content

finishWith :: Stream s m Char => String -> ParsecT s u m a -> ParsecT s u m a
finishWith str = between (char ']') (string str)

escapedRaw :: Stream s m Char => ParsecT s u m Char
escapedRaw =
      try (pure '[' <$> string "\\[")
  <|> char '\\'
  <|> noneOf "["

content :: Stream s m Char => ParsecT s u m Content
content =
  (do
    switch <- choice $ fmap (try . string . ('[':))
              [ "br" , "img" , "i" , "b" , "tt" , "url" , "center"
              , "footnote" , "span" ]
    case tail switch of
      "br"       -> pure BR      <$> char ']'
      "img"      -> IMG          <$> finishWith "[/img]"      (Url <$> many1 (try $ noneOf "["))
      "i"        -> I            <$> finishWith "[/i]"        contents
      "b"        -> B            <$> finishWith "[/b]"        contents
      "u"        -> U            <$> finishWith "[/u]"        contents
      "tt"       -> TT           <$> finishWith "[/tt]"       contents
      "center"   -> CENTER       <$> finishWith "[/center]"   contents
      "footnote" -> FOOTNOTE     <$> finishWith "[/footnote]" contents
      "url"      -> uncurry URL  <$> url
      "span"     -> uncurry SPAN <$> withTags contents <* string "[/span]"
  ) <|> RAW  <$> many1 escapedRaw


blogPost :: Stream s m Char => ParsecT s u m [Structure]
blogPost = structure `manyTill` eof

main :: FilePath -> IO ()
main fp = do
  input <- readFile fp
  print $ parse blogPost "" input
