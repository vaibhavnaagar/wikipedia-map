{-# LANGUAGE OverloadedStrings #-}
module WikiParser
    ( getWikiTitle,
      getWikiText,
      getWikiLinks,
      parseHtml
    ) where

import qualified Data.Aeson.Types as AT
import qualified Data.HashMap.Strict as HS
import qualified Data.Text as T
import Text.HTML.Parser (parseTokens, Token(..), Attr(..))
import WikiApiService (apiRequest)


-- Get the accurate title of a page by querying wikipedia API from json data
getWikiTitle :: Maybe AT.Value -> Maybe T.Text
getWikiTitle jsonData = do
  AT.Object queryValPair <- jsonData
  AT.Object pageValPair  <- HS.lookup "query" queryValPair
  AT.Object idValPair    <- HS.lookup "pages" pageValPair
  let AT.Object titleValPair = snd $ head $ HS.toList idValPair
  AT.String title        <- HS.lookup "title" titleValPair
  return title

--  Get HTML text of a wikipedia page from json data based on standard query
getWikiText :: Maybe AT.Value -> Maybe T.Text
getWikiText jsonData = do
  AT.Object parseValPair <- jsonData
  AT.Object keyValPair   <- HS.lookup "parse" parseValPair
  AT.Object textValPair  <- HS.lookup "text" keyValPair
  AT.String textVal      <- HS.lookup "*" textValPair
  return textVal

-- Get all the wiki links present in paragraphs only in the form of accurate wiki titles
getWikiLinks :: Maybe AT.Value -> Maybe [T.Text]
getWikiLinks jsonData = do
  tokens <- (parseHtml . getWikiText) jsonData
  return $ extractAllLinksFast tokens False
  -- return $ extractLinksFast tokens 2 False
  -- return $ extractLinks $ extractAllParagraphs tokens False

-- Parse the HTML text of a wikipedia page and convert each element into token
parseHtml :: Maybe T.Text -> Maybe [Token]
parseHtml htmlText = do
  text <- htmlText
  return $ parseTokens text

-- Extract the content of the first N number of paragraphs from the parsed HTML text
extractParagraphs :: (Eq a, Fractional a) => [Token] -> a -> Bool -> [Token]
extractParagraphs [] _ _ = []
extractParagraphs _ 0 _  = []
extractParagraphs (token:tokens) num False =
      let
        checkTag (TagOpen "p" _) = extractParagraphs tokens num True
        checkTag _               = extractParagraphs tokens num False
      in checkTag token
extractParagraphs (token:tokens) num True
      | token == TagClose "p" = extractParagraphs tokens (num - 1) False
      | otherwise             = token:(extractParagraphs tokens num True)

-- Extract all the paragraphs in the parsed HTML text
extractAllParagraphs :: [Token] -> Bool -> [Token]
extractAllParagraphs tokens bool = let inf = 1/0
                                   in extractParagraphs tokens inf bool

-- Extract all the links from the parsed HTML text
extractLinks :: [Token] -> [T.Text]
extractLinks [] = []
extractLinks (token:tokens) =
      let
        checkTag (TagOpen "a" ((Attr "title" wikiLink):_)) = wikiLink:extractLinks tokens
        checkTag _                                         = extractLinks tokens
      in checkTag token

-- Extract all the links present in the first N number of paragraphs in one go
extractLinksFast :: (Eq a, Fractional a) => [Token] -> a -> Bool -> [T.Text]
extractLinksFast [] _ _ = []
extractLinksFast _ 0 _  = []
extractLinksFast (token:tokens) num False =
      let
        checkTag (TagOpen "p" _) = extractLinksFast tokens num True
        checkTag _               = extractLinksFast tokens num False
      in checkTag token
extractLinksFast (token:tokens) num True
      | token == TagClose "p" = extractLinksFast tokens (num - 1) False
      | otherwise             =
        let
          checkTag (TagOpen "a" ((Attr "title" wikiLink):_)) = wikiLink:extractLinksFast tokens num True
          checkTag _                                         = extractLinksFast tokens num True
        in checkTag token

-- Extract all the links present in all the paragraphs in one go
extractAllLinksFast :: [Token] -> Bool -> [T.Text]
extractAllLinksFast tokens bool = let inf = 1/0
                                   in extractLinksFast tokens inf bool
