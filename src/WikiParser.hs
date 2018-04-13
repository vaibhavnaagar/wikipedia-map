module WikiParser
    (
    ) where

{-# LANGUAGE OverloadedStrings #-}
import Text.Pandoc (readHtml)
import Text.Pandoc.Definition (Pandoc(..), Inline(..))
import Text.Pandoc.Options (ReaderOptions(..), def)
import Text.Pandoc.Walk (query)
import Text.Pandoc.Class (runPure)
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
  let textData = (parseHtml . getWikiText) $ Just jsonData
  print textData


getWikiText :: Maybe AT.Value -> Maybe T.Text
getWikiText jsonData = do
  AT.Object parseValPair <- jsonData
  AT.Object keyValPair   <- HS.lookup (T.pack "parse") parseValPair
  AT.Object textValPair  <- HS.lookup (T.pack "text") keyValPair
  AT.String textVal      <- HS.lookup (T.pack "*") textValPair
  return textVal


parseHtml :: Maybe T.Text -> Maybe Pandoc
parseHtml htmlText = do
  text <- htmlText
  case runPure $ readHtml def{readerStripComments = True} text of
    Left _           -> Nothing
    Right parsedText -> Just parsedText

  -- case readHtml def{readerSmart = True} htmlText of
  --                     Left _           -> Nothing
  --                     Right parsedText -> Maybe parsedText

getWikiLinks :: Maybe Pandoc -> Maybe [String]
getWikiLinks text = do
  pandocText <- text
  return $ query extractURL pandocText


extractURL :: Inline -> [String]
extractURL (Link _ _ (u,_)) = [u]
-- extractURL (Image _ _ (u,_)) = [u]
extractURL _ = []
