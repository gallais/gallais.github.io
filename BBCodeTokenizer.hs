{-# OPTIONS  -Wall             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ViewPatterns      #-}

module BBCodeTokenizer where

import Data.Tuple
import Data.List
import Data.Text (Text)
import qualified Data.Text as T

import Control.Monad

import Text.Parsec
import Text.Parsec.Pos

data OpClable =
  P | H | DIV | SPAN | URL |
  UL | LI | U | I | B | TT | IMG | CENTER | FOOTNOTE
  deriving (Eq, Show)

data Token =
    RAW      Text
  | OPENTOK  OpClable
  | CLOSETOK OpClable
  | BR
  deriving (Eq, Show)

type Keywords = [(Token, Text)]

keywords :: Keywords
keywords =
  let taggable tok tex = [ (OPENTOK tok, '['  `T.cons` tex)
                         , (CLOSETOK tok, "[/" `T.append` tex `T.snoc` ']') ]
      closable tok tex = [ (OPENTOK  tok, '[' `T.cons` tex `T.snoc` ']')
                         , (CLOSETOK tok, "[/" `T.append` tex `T.snoc` ']') ]
  in
  (BR, "[br]")
  :  concatMap (uncurry taggable)
       [ (P, "p"), (H, "h"), (DIV, "div"), (SPAN, "span"), (URL, "url") ]
  ++ concatMap (uncurry closable)
       [ (UL, "ul"), (LI, "li"), (U, "u"), (I, "i"), (B, "b"), (TT , "tt"), (IMG, "img")
       , (CENTER, "center"), (FOOTNOTE, "footnote") ]

rawOrKeyword :: Keywords -> Text -> Maybe (Token, Text)
rawOrKeyword ogkeys = go T.empty ogkeys
  where
    grabRest :: Char -> Keywords -> Keywords
    grabRest c = fmap (fmap T.tail) . filter ((c ==) . T.head . snd)
    go acc _ (T.uncons -> Nothing) = T.uncons acc >> return (RAW acc, T.empty)
    go acc keys (T.uncons -> Just (c, t)) =
      let matchedRest = grabRest c keys
          matchedOG = grabRest c ogkeys in
      if null matchedRest
      then
        if null matchedOG
        then return (RAW (acc `T.snoc` c), t)
        else return (RAW acc, c `T.cons` t)
      else maybe
            (go (acc `T.snoc` c) matchedRest t)
            (\ tok -> return (tok, t))
            (lookup T.empty $ fmap swap matchedRest)

tokenize :: Text -> Maybe (Token, Text)
tokenize = rawOrKeyword keywords

tokenizer :: Text -> [Token]
tokenizer = collapseRaws . unfoldr tokenize

collapseRaws :: [Token] -> [Token]
collapseRaws (RAW x : RAW y : xs) = collapseRaws $ RAW (x `T.append` y) : xs
collapseRaws []     = []
collapseRaws (x:xs) = x : collapseRaws xs

updatePosToken :: SourcePos -> Token -> SourcePos
updatePosToken sp = updatePosString sp . T.unpack . getText
  where
    getText (RAW txt) = txt
    getText anything  = maybe "" id $ lookup anything keywords

isExactly :: (Stream s m Token) => Token -> ParsecT s u m Token
isExactly t = tokenPrim show (\ s -> const . (updatePosToken s))
                             (\ u -> guard (t == u) >> Just t)

isRaw :: (Stream s m Token) => ParsecT s u m Text
isRaw = tokenPrim show (\ s -> const . (updatePosToken s)) fromRaw
  where
    fromRaw (RAW txt) = Just txt
    fromRaw _         = Nothing

isOneOf :: (Stream s m Token) => [Token] -> ParsecT s u m Token
isOneOf = choice . fmap isExactly
