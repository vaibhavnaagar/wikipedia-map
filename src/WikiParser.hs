{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module WikiParser
    ( getWikiTitle
    , getWikiText
    , getWikiLinks
    , getFirstNWikiLinks
    , getRandomWiki
    , parseHtml
    , WikiPage(..)
    , WikiText(..)
    , WikiLinks(..)
    ) where

import qualified Data.Aeson.Types as AT
import qualified Data.HashMap.Strict as HS
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.List (nub)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Text.HTML.Parser (parseTokens, Token(..), Attr(..))
import WikiApiService (apiRequest)


-- Data types
data WikiPage = WikiPage
  { wikiTitle :: T.Text,
    wikiText  :: T.Text,
    wikiLinks :: [T.Text]
  } deriving (Generic, Show)

instance FromJSON WikiPage
instance ToJSON WikiPage

newtype WikiText  = WikiText T.Text deriving (Generic, Show)
newtype WikiLinks = WikiLinks [T.Text] deriving (Generic, Show)

instance ToJSON WikiText
instance ToJSON WikiLinks


-- Get the accurate title of a page by querying wikipedia API from json data
getWikiTitle :: Maybe AT.Value -> Maybe WikiText
getWikiTitle jsonData = do
  AT.Object queryValPair <- jsonData
  AT.Object pageValPair  <- HS.lookup "query" queryValPair
  AT.Object idValPair    <- HS.lookup "pages" pageValPair
  let AT.Object titleValPair = snd $ head $ HS.toList idValPair
  AT.String title        <- HS.lookup "title" titleValPair
  return $ WikiText title

--  Get HTML text of a wikipedia page from json data based on standard query
getWikiText :: Maybe AT.Value -> Maybe WikiText
getWikiText jsonData = do
  AT.Object parseValPair <- jsonData
  AT.Object keyValPair   <- HS.lookup "parse" parseValPair
  AT.Object textValPair  <- HS.lookup "text" keyValPair
  AT.String textVal      <- HS.lookup "*" textValPair
  return $ WikiText textVal

-- Get all the wiki links present in paragraphs only in the form of accurate wiki titles
getWikiLinks :: Maybe AT.Value -> Maybe WikiLinks
getWikiLinks jsonData = do
  WikiText htmlText <- getWikiText jsonData
  tokens            <- parseHtml $ Just htmlText
  -- return $ WikiLinks $ extractAllLinksFast tokens
  return $ WikiLinks $ extractAllNonParenLinks tokens
  -- return $ WikiLinks $ extractNonParenLinks tokens 1 False 0
  -- return $ WikiLinks $ extractLinksFast tokens 2 False
  -- return $ WikiLinks $ extractLinks $ extractAllParagraphs tokens False

-- Get first N wiki links present in paragraphs only in the form of accurate wiki titles
getFirstNWikiLinks :: Maybe AT.Value -> Int -> Maybe WikiLinks
getFirstNWikiLinks jsonData n = do
  WikiText htmlText <- getWikiText jsonData
  tokens            <- parseHtml $ Just htmlText
  -- return $ WikiLinks $ take n $ extractAllLinksFast tokens
  return $ WikiLinks $ take n $ extractAllNonParenLinks tokens

--  Get a random wiki title from the jsonData (specific to a query)
getRandomWiki :: Maybe AT.Value -> Maybe WikiText
getRandomWiki jsonData = do
  AT.Object queryValPair <- jsonData
  AT.Object random       <- HS.lookup "query" queryValPair
  AT.Array pageList      <- HS.lookup "random" random
  let AT.Object pages    = V.head pageList
  AT.String title        <- HS.lookup "title" pages
  return $ WikiText title

-- Parse the HTML text of a wikipedia page and convert each element into token
parseHtml :: Maybe T.Text -> Maybe [Token]
parseHtml htmlText = do
  text <- htmlText
  return $ parseTokens text

-- Extract the content of the first N number of paragraphs from the parsed HTML text
extractParagraphs :: (Eq a, Fractional a) => [Token] -> a -> Bool -> [Token]
extractParagraphs [] _ _ = []
extractParagraphs _ 0 _  = []
extractParagraphs (token:tokens) num False = case token of
      TagOpen "p" _ -> extractParagraphs tokens num True
      otherwise     -> extractParagraphs tokens num False
extractParagraphs (token:tokens) num True = case token of
      TagClose "p" -> extractParagraphs tokens (num - 1) False
      otherwise    -> token:(extractParagraphs tokens num True)

-- Extract all the paragraphs in the parsed HTML text
extractAllParagraphs :: [Token] -> [Token]
extractAllParagraphs tokens = let inf = 1/0
                              in extractParagraphs tokens inf False

-- Extract all the wiki links from the parsed HTML text
extractLinks :: [Token] -> [T.Text]
extractLinks [] = []
extractLinks (token:tokens) = case token of
      TagOpen "a" attributes -> case extractTitleFromLink attributes of
            Just wikiLink -> wikiLink:extractLinks tokens
            otherwise     -> extractLinks tokens
      otherwise              -> extractLinks tokens

-- Extract title from a list of attributes of a hyperlink tag if it is a wikipedia link
extractTitleFromLink :: [Attr] -> Maybe T.Text
extractTitleFromLink [] = Nothing
extractTitleFromLink (attribute:attributes) = case attribute of
      Attr "title" wikiLink -> Just wikiLink
      otherwise             -> extractTitleFromLink attributes

-- Extract all the links present in the first N number of paragraphs in one go
extractLinksFast :: (Eq a, Fractional a) => [Token] -> a -> Bool -> [T.Text]
extractLinksFast [] _ _ = []
extractLinksFast _ 0 _  = []
extractLinksFast (token:tokens) num False = case token of
      TagOpen "p" _ -> extractLinksFast tokens num True
      otherwise     -> extractLinksFast tokens num False
extractLinksFast (token:tokens) num True = case token of
      TagClose "p" -> extractLinksFast tokens (num - 1) False
      otherwise    -> case token of
            TagOpen "a" attributes -> case extractTitleFromLink attributes of
                  Just wikiLink -> wikiLink:extractLinksFast tokens num True
                  otherwise     -> extractLinksFast tokens num True
            otherwise              -> extractLinksFast tokens num True

-- Extract all the links present in all the paragraphs in one go
extractAllLinksFast :: [Token] -> [T.Text]
extractAllLinksFast tokens =  let inf = 1/0
                              in extractLinksFast tokens inf False


-- Extract all the links (not in any parenthesis) present in the first N number of paragraphs in one go
--  extractNonParenLinks :: tokens -> numParagraphs -> insideParagraph -> numOpenParenthesis -> List of links
extractNonParenLinks :: (Eq a, Fractional a) => [Token] -> a -> Bool -> Int -> [T.Text]
extractNonParenLinks [] _ _ _ = []
extractNonParenLinks _ 0 _ _ = []
extractNonParenLinks (token:tokens) num False numParen = case token of
     TagOpen "p" _ -> extractNonParenLinks tokens num True numParen
     ContentText c -> extractNonParenLinks tokens num False $ numOpenParen c numParen
     otherwise     -> extractNonParenLinks tokens num False numParen
extractNonParenLinks (token:tokens) num True numParen = case token of
     TagClose "p"  -> extractNonParenLinks tokens (num - 1) False numParen
     ContentText c -> extractNonParenLinks tokens num True $ numOpenParen c numParen
     otherwise     -> if (numParen > 0) then extractNonParenLinks tokens num True numParen else case token of
           TagOpen "a" attributes -> case extractTitleFromLink attributes of
                 Just wikiLink -> wikiLink:extractNonParenLinks tokens num True numParen
                 otherwise     -> extractNonParenLinks tokens num True numParen
           otherwise              -> extractNonParenLinks tokens num True numParen

-- Extract all the links (not in any parenthesis) present in all the paragraphs in one go
extractAllNonParenLinks :: [Token] -> [T.Text]
extractAllNonParenLinks tokens =  let inf = 1/0
                                  in nub $ extractNonParenLinks tokens inf False 0


-- Remove duplicate links from the list
removeDups :: [T.Text] -> [T.Text]
removeDups = foldl (\ links link -> if link `elem` links
                                    then links
                                    else links ++ [link]) []

-- Count number of open parenthesis in a text
numOpenParen :: T.Text -> Int -> Int
numOpenParen "" count = count
numOpenParen str count
      | T.head str == '(' = numOpenParen (T.tail str) (count + 1)
      | T.head str == ')' = if (count == 0) then numOpenParen (T.tail str) 0 else numOpenParen (T.tail str) (count - 1)
      | otherwise         = numOpenParen (T.tail str) count
