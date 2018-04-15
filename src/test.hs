import WikiApiService (apiRequest)
import WikiParser (getWikiTitle, getWikiLinks, getWikiText, getRandomWiki, WikiText(..), WikiLinks(..))


testapi :: IO ()
testapi = do
  jsonData <- apiRequest [("format", "json"), ("action", "query"), ("list", "random"), ("rnlimit", "1"), ("rnnamespace", "0")]
  -- jsonData <- apiRequest [("format", "json"), ("action", "parse"), ("prop", "text"), ("section", "0"), ("page", "christmas day"), ("redirects", "1")]
  -- jsonData <- apiRequest [("format", "json"), ("action", "query"), ("titles", "christmas day"), ("redirects", "1")]
  print jsonData
  let title = getRandomWiki $ Just jsonData
  print title
  -- let textData = case getWikiLinks $ Just jsonData of
  --               Nothing -> WikiLinks []
  --               Just t  -> t
  -- print textData
  -- print $ length textData
