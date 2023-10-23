{-# OPTIONS  -Wall             #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Text.BBCode.Parser where

import Control.Applicative hiding (many, (<|>))
import Control.Monad

import Data.Char
import Data.Text (Text)
import qualified Data.Text    as T

import           Text.BBCode.Tokenizer (Token)
import qualified Text.BBCode.Tokenizer as Tok

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
  | FOOTNOTE     [Structure]
  | URL      Url [Content]
  | IMG      Url
  | RAW     Text
 deriving Show

finishesWith :: Stream s m Token => ParsecT s u m a -> Tok.OpClable -> ParsecT s u m a
finishesWith p elt = p <* Tok.isExactly (Tok.CLOSETOK elt)

withDigit :: Stream s m Token => (Text -> [a] -> [a]) -> ParsecT s u m [a] -> ParsecT s u m (Int, [a])
withDigit emb p = Tok.isRaw >>= \ txt -> dig txt <$> p
  where
    dig txt xs = uncurry (\ pr sf -> (read $ T.unpack pr, emb (T.tail sf) xs)) $ T.breakOn "]" txt

withTags :: Stream s m Token => (Text -> [a] -> [a]) -> ParsecT s u m [a] -> ParsecT s u m (Tag, [a])
withTags emb p = Tok.isRaw >>= \ txt -> tag txt <$> p
  where

    getRest :: (Name -> Tag) -> Text -> (Tag, Text)
    getRest c txt = uncurry (\ pr sf -> (c $ Name pr , T.tail sf)) $ T.breakOn "]" txt

    tag (T.stripPrefix "id=" -> Just txt) xs = fmap (flip emb xs) $ getRest ID    txt
    tag (T.stripPrefix "="   -> Just txt) xs = fmap (flip emb xs) $ getRest CLASS txt
    tag (T.stripPrefix "]"   -> Just txt) xs = (NULL, emb txt xs)
     -- todo: fallback to textual representation in case we have e.g.
     -- "[span" but nothing appropriate right after it.

url :: Stream s m Token => ParsecT s u m (Url, [Content])
url = do
  (link, rest1) <- T.breakOn "]" <$> Tok.isRaw
  rest2         <- many content `finishesWith` Tok.URL
  return (Url link, smart RAW (T.tail rest1) rest2)

contents :: Stream s m Token => ParsecT s u m [Content]
contents = many1 content

-- smart constructor
smart :: (Text -> a) -> Text -> [a] -> [a]
smart emb txt xs
  | T.null txt = xs
  | otherwise  = emb txt : xs

ul :: Stream s m Token => ParsecT s u m [[Content]]
ul = do
  _ <- many (try (Tok.isRaw >>= guard . T.all isSpace))
  _   <- Tok.isExactly (Tok.OPENTOK Tok.LI)
  li  <- contents `finishesWith` Tok.LI
  rec <- try ul <|> (many content `finishesWith` Tok.UL *> pure [])
  return $ li : rec


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
         Tok.FOOTNOTE -> FOOTNOTE     <$> structures          `finishesWith` Tok.FOOTNOTE
         Tok.URL      -> uncurry URL  <$> url
         Tok.SPAN     -> uncurry SPAN <$> withTags (smart RAW) (many content `finishesWith` Tok.SPAN)
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
        Tok.UL  -> UL          <$> ul
  ) <|> TXT <$> contents

blogPost :: Stream s m Token => ParsecT s u m [Structure]
blogPost = structure `manyTill` eof

parseBBCode :: Text -> Either ParseError [Structure]
parseBBCode = parse blogPost "" . Tok.tokenize
