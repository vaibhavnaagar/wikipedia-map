module API
    ( someFunc
    ) where

import WikiApiService (apiRequest)
import WikiParser (getWikiTitle, getWikiLinks, getWikiText)


data WikiPage = Page
  { endpoint :: String,
    format :: String,
    action :: String,
    titles :: [String],
    prop :: String,
    redirects :: Int
  } deriving (Show)

testapi :: IO ()
testapi = do
  jsonData <- apiRequest [("format", "json"), ("action", "parse"), ("prop", "text"), ("section", "0"), ("page", "christmas day"), ("redirects", "1")]
  -- jsonData <- apiRequest [("format", "json"), ("action", "query"), ("titles", "christmas day"), ("redirects", "1")]
  -- print jsonData
  -- let title = getWikiTitle $ Just jsonData
  -- print title
  let textData = case getWikiLinks $ Just jsonData of
                Nothing -> []
                Just t  -> t
  print textData
  print $ length textData

someFunc :: IO ()
someFunc = putStrLn "someFunc"
