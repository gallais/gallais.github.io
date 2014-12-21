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
    DIV Tag Structure
  | H   Int [Content]
  | P   Tag Structure
  | UL      [Content]
  | TXT     [Content]
 deriving Show

data Content =
    I            [Content]
  | B            [Content]
  | U            [Content]
  | TT           [Content]
  | SPAN     Tag [Content]
  | FOOTNOTE     [Content]
  | URL      Url [Content]
  | RAW     String
 deriving Show

sandwichedBy :: Stream s m Char => ParsecT s u m a -> String -> ParsecT s u m a
sandwichedBy p str = between (string $ '[' : str ++ "]") (string $ "[/" ++ str ++ "]") p

sandwichedWithTagsBy :: Stream s m Char => ParsecT s u m a -> String -> ParsecT s u m (Tag, a)
sandwichedWithTagsBy p str = do
  _    <- string $ '[' : str
  arg  <- tag
  rest <- p
  _    <- string $ "[/" ++ str ++ "]"
  return (arg, rest)

url :: Stream s m Char => ParsecT s u m (Url, [Content])
url = do
  link <- between (char '=') (char ']') $ many (try $ noneOf "]")
  rest <- many content
  _    <- string "[/url]"
  return (Url link, rest)

name :: Stream s m Char => ParsecT s u m Name
name = Name <$> many alphaNum

tag :: Stream s m Char => ParsecT s u m Tag
tag =
  choice
    [ ID         <$> between (string "id=") (char ']') name
    , CLASS      <$> between (string "=")   (char ']') name
    , const NULL <$> string "]"
    ]

withTags :: Stream s m Char => ParsecT s u m a -> ParsecT s u m (Tag, a)
withTags p = (,) <$> tag <*> p

list :: Stream s m Char => ParsecT s u m [Content]
list = many $ content `sandwichedBy` "li"

structure :: Stream s m Char => ParsecT s u m Structure
structure =
  choice $ fmap try
     [ uncurry DIV <$> structure    `sandwichedWithTagsBy` "div"
     , H 3         <$> many content `sandwichedBy`    "h3"
     , H 4         <$> many content `sandwichedBy`    "h4"
     , uncurry P   <$> structure    `sandwichedWithTagsBy` "p"
     , UL          <$> list    `sandwichedBy`    "ul"
     , TXT         <$> many content
     , newline     *> structure
     ]

contents :: Stream s m Char => ParsecT s u m [Content]
contents = many content

finishWith :: Stream s m Char => String -> ParsecT s u m a -> ParsecT s u m a
finishWith str = between (char ']') (string str)

content :: Stream s m Char => ParsecT s u m Content
content =
  (do
    switch <- choice $ fmap (try . string . ('[':))
              [ "i" , "b" , "tt" , "url" , "footnote" , "span" ]
    case tail switch of
      "i"        -> I            <$> finishWith "[/i]"        contents
      "b"        -> B            <$> finishWith "[/b]"        contents
      "u"        -> U            <$> finishWith "[/u]"        contents
      "tt"       -> TT           <$> finishWith "[/tt]"       contents
      "footnote" -> FOOTNOTE     <$> finishWith "[/footnote]" contents
      "url"      -> uncurry URL  <$> url
      "span"     -> uncurry SPAN <$> withTags contents <* string "[/span]"
  ) <|> RAW  <$> many1 (try $ noneOf "[")

blogPost :: Stream s m Char => ParsecT s u m [Content]
blogPost = content `manyTill` eof

main :: FilePath -> IO ()
main fp = do
  input <- readFile fp
  print $ parse blogPost "" input
