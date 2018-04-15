{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
import WikiApiService (apiRequest)
import WikiParser (getWikiTitle, getWikiLinks, getWikiText, getRandomWiki, parseHtml, WikiText(..), WikiLinks(..))


testapi :: IO ()
testapi = do
  -- jsonData <- apiRequest [("format", "json"), ("action", "query"), ("list", "random"), ("rnlimit", "1"), ("rnnamespace", "0")]
  jsonData <- apiRequest [("format", "json"), ("action", "parse"), ("prop", "text"), ("section", "0"), ("page", "mathematics"), ("redirects", "1")]
  -- jsonData <- apiRequest [("format", "json"), ("action", "query"), ("titles", "christmas day"), ("redirects", "1")]
  -- print jsonData
  let WikiText htmlText = case getWikiText $ Just jsonData of
                          Nothing -> WikiText ""
                          Just wt -> wt
  let tokens            = parseHtml $ Just htmlText
  print tokens
  -- let title = getRandomWiki $ Just jsonData
  -- print title
  let textData = case getWikiLinks $ Just jsonData of
                Nothing -> WikiLinks []
                Just t  -> t
  print textData
  -- print $ length textData
