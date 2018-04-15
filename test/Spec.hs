{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
import qualified Data.Text as T
import WikiParser (getWikiTitle, getWikiLinks, getFirstNWikiLinks, getRandomWiki, parseHtml, WikiText(..), WikiLinks(..))
import WikiApiService (apiRequest)
import ServeApi (app, generateQuery, WikiParams(..))


gettingToPage :: T.Text -> T.Text -> [T.Text] -> IO (String)
gettingToPage startPage endPage visitedPages
      | startPage `elem` visitedPages = return $ "Loop Detected! :("
      | startPage == endPage = return $ "Yay! Finally reached to \"" ++ T.unpack endPage ++ "\""
      | otherwise = do
        putStrLn $ T.unpack startPage
        let queries = generateQuery $ WikiParams  { formatW      = "json"
                                                  , actionW      = "parse"
                                                  , titlesW      = ""
                                                  , pageW        = T.unpack startPage
                                                  , propW        = "text"
                                                  , listW        = ""
                                                  , sectionW     = "0"
                                                  , redirectsW   = "1"
                                                  , rnlimitW     = ""
                                                  , rnnamespaceW = ""
                                                  }
        jsonData <- apiRequest queries
        case getFirstNWikiLinks (Just jsonData) 1 of
          Nothing    -> return $ "Unable to get the first link from the page: \"" ++ T.unpack startPage ++ "\""
          Just (WikiLinks links) -> gettingToPage (head links) endPage (visitedPages ++ [startPage])


getAccurateTitle :: String -> IO (Maybe T.Text)
getAccurateTitle page = do
  let queries = generateQuery $ WikiParams  { formatW      = "json"
                                            , actionW      = "query"
                                            , titlesW      = page
                                            , pageW        = ""
                                            , propW        = ""
                                            , listW        = ""
                                            , sectionW     = ""
                                            , redirectsW   = "1"
                                            , rnlimitW     = ""
                                            , rnnamespaceW = ""
                                            }
  jsonData <- apiRequest queries
  case getWikiTitle $ Just jsonData of
    Nothing               -> return Nothing
    Just (WikiText title) -> return $ Just title

main :: IO ()
main = do
  putStrLn "Enter the start page"
  spage <- getLine
  putStrLn "Enter the end page"
  epage <- getLine
  page1 <- getAccurateTitle spage
  page2 <- getAccurateTitle epage
  putStrLn "Let the journey begin..."
  putStrLn ""
  message <- case page1 of
              Nothing    -> return "Invalid Page Names"
              Just start -> case page2 of
                Nothing  -> return "Invalid Page Names"
                Just end -> gettingToPage start end []
  putStrLn message
