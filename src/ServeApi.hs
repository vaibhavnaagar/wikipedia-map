{-# LANGUAGE OverloadedStrings, DataKinds, TypeOperators  #-}
module ServeApi
    ( app
    ) where

import qualified Data.Text as T
import Servant
import Network.Wai
import Control.Monad.IO.Class (liftIO)
import WikiApiService (apiRequest)
import WikiParser (getWikiTitle, getWikiLinks, getWikiText, WikiTitle(..), WikiText(..), WikiLinks(..))


data WikiParams = WikiParams
  { formatW      :: String
  , actionW      :: String
  , titlesW      :: String
  , pageW        :: String
  , propW        :: String
  , listW        :: String
  , sectionW     :: String
  , redirectsW   :: String
  , rnlimitW     :: String
  , rnnamespaceW :: String
  } deriving (Show)


testapi :: IO ()
testapi = do
  jsonData <- apiRequest [("format", "json"), ("action", "parse"), ("prop", "text"), ("section", "0"), ("page", "christmas day"), ("redirects", "1")]
  -- jsonData <- apiRequest [("format", "json"), ("action", "query"), ("titles", "christmas day"), ("redirects", "1")]
  -- print jsonData
  -- let title = getWikiTitle $ Just jsonData
  -- print title
  let textData = case getWikiLinks $ Just jsonData of
                Nothing -> WikiLinks []
                Just t  -> t
  print textData
  -- print $ length textData


type WikiApi = "links" :> QueryParam "page" String :> Get '[JSON] (Headers '[Header "Access-Control-Allow-Origin" String, Header "Access-Control-Allow-Headers" String] WikiLinks)
          :<|> "pagename" :> QueryParam "page" String :> Get '[JSON] (Headers '[Header "Access-Control-Allow-Origin" String, Header "Access-Control-Allow-Headers" String] WikiTitle)
          -- :<|> "random" :> Get '[JSON] WikiRandom

type ResponseType = Headers '[Header "Access-Control-Allow-Origin" String, Header "Access-Control-Allow-Headers" String]

wikiServer :: Server WikiApi
wikiServer = getLinks
        :<|> getTitle
        -- :<|> getRandomWiki


app :: Application
app = serve myApi wikiServer
  where
    myApi :: Proxy WikiApi
    myApi = Proxy :: Proxy WikiApi


paramValuePair :: String -> String -> [(String, String)]
paramValuePair key "" = []
paramValuePair key value = [(key, value)]


generateQuery :: WikiParams -> [(String, String)]
generateQuery params = let
  format      = paramValuePair "format" $ formatW params
  action      = paramValuePair "action" $ actionW params
  prop        = paramValuePair "prop" $ propW params
  section     = paramValuePair "section" $ sectionW params
  page        = paramValuePair "page" $ pageW params
  titles      = paramValuePair "titles" $ titlesW params
  list        = paramValuePair "list" $ listW params
  redirects   = paramValuePair "redirects" $ redirectsW params
  rnlimit     = paramValuePair "rnlimit" $ rnlimitW params
  rnnamespace = paramValuePair "rnnamespace" $ rnnamespaceW params
  in format ++ action ++ prop ++ section ++ page ++ titles ++ list ++ redirects ++ rnlimit ++ rnnamespace


getLinks :: Maybe String -> Handler (ResponseType WikiLinks)
getLinks maybePage = case maybePage of
  Nothing    -> return $ addHeader "*" $ addHeader "Origin, X-Requested-With, Content-Type, Accept" $ WikiLinks []
  Just page  -> do
        let queries = generateQuery $ WikiParams  { formatW      = "json"
                                                  , actionW      = "parse"
                                                  , titlesW      = ""
                                                  , pageW        = page
                                                  , propW        = "text"
                                                  , listW        = ""
                                                  , sectionW     = "0"
                                                  , redirectsW   = "1"
                                                  , rnlimitW     = ""
                                                  , rnnamespaceW = ""
                                                  }
        jsonData <- liftIO (apiRequest queries)
        case getWikiLinks $ Just jsonData of
          Nothing     -> return $ addHeader "*" $ addHeader "Origin, X-Requested-With, Content-Type, Accept" $ WikiLinks []
          Just links  -> return $ addHeader "*" $ addHeader "Origin, X-Requested-With, Content-Type, Accept" links


getTitle :: Maybe String -> Handler (ResponseType WikiTitle)
getTitle maybePage = case maybePage of
  Nothing    -> return $ addHeader "*" $ addHeader "Origin, X-Requested-With, Content-Type, Accept" $ WikiTitle "Unknown Title"
  Just page  -> do
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
        jsonData <- liftIO (apiRequest queries)
        case getWikiTitle $ Just jsonData of
            Nothing     -> return $ addHeader "*" $ addHeader "Origin, X-Requested-With, Content-Type, Accept" $ WikiTitle "Unknown Title"
            Just title  -> return $ addHeader "*" $ addHeader "Origin, X-Requested-With, Content-Type, Accept" title
