{-# LANGUAGE OverloadedStrings #-}
module WikiParser
    (
    ) where

import qualified Data.Aeson.Types as AT
import qualified Data.HashMap.Strict as HS
import qualified Data.Text as T
import Text.HTML.Parser (parseTokens, Token(..), Attr(..))
import WikiApiService (apiRequest)

data WikiPage = Page
  { endpoint :: String,
    format :: String,
    action :: String,
    titles :: [String],
    prop :: String,
    redirects :: Int
  } deriving (Show)

-- get
-- getPageName :: [String] -> [String]
-- getPageName =

testapi :: IO ()
testapi = do
  jsonData <- apiRequest [("format", "json"), ("action", "parse"), ("prop", "text"), ("section", "0"), ("page", "pizza")]
  let textData = case (getWikiLinks . parseHtml . getWikiText) $ Just jsonData of
                Nothing -> []
                Just t  -> t
  print textData
  print $ length textData


getWikiText :: Maybe AT.Value -> Maybe T.Text
getWikiText jsonData = do
  AT.Object parseValPair <- jsonData
  AT.Object keyValPair   <- HS.lookup "parse" parseValPair
  AT.Object textValPair  <- HS.lookup "text" keyValPair
  AT.String textVal      <- HS.lookup "*" textValPair
  return textVal


parseHtml :: Maybe T.Text -> Maybe [Token]
parseHtml htmlText = do
  text <- htmlText
  return $ parseTokens text


getWikiLinks :: Maybe [Token] -> Maybe [T.Text]
getWikiLinks maybeTokens = do
  tokens <- maybeTokens
  return $ extractLinks $ extractAllParagraphs tokens False


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


extractAllParagraphs :: [Token] -> Bool -> [Token]
extractAllParagraphs tokens bool = let inf = 1/0
                                   in extractParagraphs tokens inf bool


extractLinks :: [Token] -> [T.Text]
extractLinks [] = []
extractLinks (token:tokens) =
      let
        checkTag (TagOpen "a" ((Attr "title" wikiLink):_)) = wikiLink:extractLinks tokens
        checkTag _                                         = extractLinks tokens
      in checkTag token
