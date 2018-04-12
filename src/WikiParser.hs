module WikiParser
    (
    ) where

{-# LANGUAGE OverloadedStrings #-}
import Text.Pandoc (readHtml)
import Text.Pandoc.Definition (Pandoc(..), Inline(..))
import Text.Pandoc.Options (Readeroptions(..))
import Text.Pandoc.Walk (query)
import qualified Data.Aeson.Types as AT
import qualified Data.HashMap.Strict as HS
import qualified Data.Text as T
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
  let textData = getWikiText $ Just jsonData
  print textData


getWikiText :: Maybe AT.Value -> Maybe String
getWikiText jsonData = do
  AT.Object parseValPair <- jsonData
  AT.Object keyValPair   <- HS.lookup (T.pack "parse") parseValPair
  AT.Object textValPair  <- HS.lookup (T.pack "text") keyValPair
  AT.String textVal      <- HS.lookup (T.pack "*") textValPair
  return $ T.unpack textVal


parseHtml :: Maybe String -> Maybe Pandoc
parseHtml htmlText = do
  text <- htmlText
  parsedText <- readHtml def{readerSmart = True} htmlText
  return parsedText

  -- case readHtml def{readerSmart = True} htmlText of
  --                     Left _           -> Nothing
  --                     Right parsedText -> Maybe parsedText


extractURL :: Inline -> [String]
extractURL (Link _ _ (u,_)) = [u]
-- extractURL (Image _ _ (u,_)) = [u]
extractURL _ = []

getWikiLinks :: Maybe Pandoc -> Maybe [String]
getWikiLinks text = do
  pandocText <- text
  return (query extractURL) . pandocText
