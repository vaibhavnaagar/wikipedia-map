{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module WikiParser
    ( getWikiTitle
    , getWikiText
    , getWikiLinks
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
  return $ WikiLinks $ extractAllLinksFast tokens False
  -- return $ extractLinksFast tokens 2 False
  -- return $ extractLinks $ extractAllParagraphs tokens False

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
extractAllParagraphs :: [Token] -> Bool -> [Token]
extractAllParagraphs tokens bool = let inf = 1/0
                                   in extractParagraphs tokens inf bool

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
extractAllLinksFast :: [Token] -> Bool -> [T.Text]
extractAllLinksFast tokens bool = let inf = 1/0
                                   in extractLinksFast tokens inf bool
