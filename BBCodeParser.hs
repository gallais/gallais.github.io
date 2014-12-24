{-# OPTIONS  -Wall             #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module BBCodeParser where

import Control.Applicative hiding (many, (<|>))
import Data.Text (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

import           BBCodeTokenizer (Token)
import qualified BBCodeTokenizer as Tok

import Text.Parsec


newtype Name = Name { outName :: Text }
 deriving Show

newtype Url  = Url  { outUrl  :: Text }
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
  | RAW     Text
 deriving Show

finishesWith :: Stream s m Token => ParsecT s u m a -> Tok.OpClable -> ParsecT s u m a
finishesWith p elt = p <* Tok.isExactly (Tok.CLOSETOK elt)

withDigit :: Stream s m Token => (Text -> [a] -> [a]) -> ParsecT s u m [a] -> ParsecT s u m (Int, [a])
withDigit emb p = Tok.isRaw >>= \ txt -> dig txt <$> p
  where
    dig txt xs = uncurry (\ pr sf -> (read $ T.unpack pr, emb sf xs)) $ T.breakOn "]" txt

withTags :: Stream s m Token => (Text -> [a] -> [a]) -> ParsecT s u m [a] -> ParsecT s u m (Tag, [a])
withTags emb p = Tok.isRaw >>= \ txt -> tag txt <$> p
  where

    getRest :: (Name -> Tag) -> Text -> (Tag, Text)
    getRest c txt = uncurry (\ pr sf -> (c $ Name pr , sf)) $ T.breakOn "]" txt

    tag (T.stripPrefix "id=" -> Just txt) xs = fmap (flip emb xs) $ getRest ID    txt
    tag (T.stripPrefix "="   -> Just txt) xs = fmap (flip emb xs) $ getRest CLASS txt
    tag (T.stripPrefix "]"   -> Just txt) xs = (NULL, emb txt xs)

contents :: Stream s m Token => ParsecT s u m [Content]
contents = many1 content

-- smart constructor
smart :: (Text -> a) -> Text -> [a] -> [a]
smart emb txt xs
  | T.null txt = xs
  | otherwise  = emb txt : xs

content :: Stream s m Token => ParsecT s u m Content
content =
  (do
     switch <- Tok.isOneOf $ Tok.BR : fmap Tok.OPENTOK
       [ Tok.IMG , Tok.I , Tok.B , Tok.U , Tok.TT , Tok.CENTER , Tok.FOOTNOTE
       , Tok.URL , Tok.SPAN ]
     case switch of
       Tok.BR          -> pure BR
       Tok.OPENTOK tok -> case tok of
         Tok.IMG      -> IMG          <$> (Url <$> Tok.isRaw) `finishesWith` Tok.IMG
         Tok.I        -> I            <$> contents            `finishesWith` Tok.I
         Tok.B        -> B            <$> contents            `finishesWith` Tok.B
         Tok.U        -> U            <$> contents            `finishesWith` Tok.U
         Tok.TT       -> TT           <$> contents            `finishesWith` Tok.TT
         Tok.CENTER   -> CENTER       <$> contents            `finishesWith` Tok.CENTER
         Tok.FOOTNOTE -> FOOTNOTE     <$> contents            `finishesWith` Tok.FOOTNOTE
         Tok.URL      -> undefined
         Tok.SPAN     -> uncurry SPAN <$> withTags (smart RAW) contents `finishesWith` Tok.SPAN
  ) <|> RAW . T.concat <$> many1 Tok.isRaw

structures :: Stream s m Token => ParsecT s u m [Structure]
structures = many1 structure

structure :: Stream s m Token => ParsecT s u m Structure
structure =
  let txt = TXT . (:[]) . RAW in
  (do
    switch <- Tok.isOneOf $ fmap Tok.OPENTOK
      [ Tok.DIV , Tok.H , Tok.P , Tok.UL ]
    case switch of
      Tok.OPENTOK tok -> case tok of
        Tok.DIV -> uncurry DIV <$> withTags  (smart txt) (many structure `finishesWith` Tok.DIV)
        Tok.H   -> uncurry H   <$> withDigit (smart RAW) (many content   `finishesWith` Tok.H)
        Tok.P   -> uncurry P   <$> withTags  (smart txt) (many structure `finishesWith` Tok.P)
        Tok.UL  -> undefined
  ) <|> TXT <$> contents


--
--  (do
--    switch <- choice $ fmap (try . string . ('[':))
--              [ "div" , "h" , "p" , "ul" ]
--    case tail switch of
--      "div" -> uncurry DIV <$> withTags structures <* string "[/div]"
--      "h"   -> H           <$> (read . (: []) <$> digit) <*> finishWith "[/h]" contents
--      "p"   -> uncurry P   <$> withTags structures <* string "[/p]"
--      "ul"  -> UL          <$> (char ']' *> ul)
--  ) <|> TXT <$> contents

{-
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

finishWith :: Stream s m Char => Text -> ParsecT s u m a -> ParsecT s u m a
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
-}

main :: FilePath -> IO ()
main fp = do
  input <- TIO.readFile fp
  print $ parse (structure `manyTill` eof) "" $ Tok.tokenizer input
