{-# LANGUAGE OverloadedStrings, DataKinds, TypeOperators  #-}
module ServeApi
    ( app
    ) where

import qualified Data.Text as T
import Servant
import Network.Wai
import Control.Monad.IO.Class (liftIO)
import WikiApiService (apiRequest)
import WikiParser (getWikiTitle, getWikiLinks, getWikiText, getRandomWiki, WikiText(..), WikiLinks(..))

-- Data Types
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

-- API type and its endpoints
type WikiApi = "links" :> QueryParam "page" String :> Get '[JSON] (Headers '[Header "Access-Control-Allow-Origin" String, Header "Access-Control-Allow-Headers" String] WikiLinks)
          :<|> "pagename" :> QueryParam "page" String :> Get '[JSON] (Headers '[Header "Access-Control-Allow-Origin" String, Header "Access-Control-Allow-Headers" String] WikiText)
          :<|> "random" :> Get '[JSON] (Headers '[Header "Access-Control-Allow-Origin" String, Header "Access-Control-Allow-Headers" String] WikiText)

-- type of response headers
type ResponseType = Headers '[Header "Access-Control-Allow-Origin" String, Header "Access-Control-Allow-Headers" String]

-- server handles request using these methods (in the same order as of endpoints)
wikiServer :: Server WikiApi
wikiServer = getLinks
        :<|> getTitle
        :<|> getRandom

-- Application to run API
app :: Application
app = serve myApi wikiServer
  where
    myApi :: Proxy WikiApi
    myApi = Proxy :: Proxy WikiApi

-- Convert key and value into list of a pair only if value is non empty string
paramValuePair :: String -> String -> [(String, String)]
paramValuePair key "" = []
paramValuePair key value = [(key, value)]

-- Generate queries using wiki params
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

-- Server call this method for "/links" request and return all wiki links of a requested page
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

-- Server call this method for "/pagename" request and return accurate wiki name of a requested page
getTitle :: Maybe String -> Handler (ResponseType WikiText)
getTitle maybePage = case maybePage of
  Nothing    -> return $ addHeader "*" $ addHeader "Origin, X-Requested-With, Content-Type, Accept" $ WikiText "Unknown Title"
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
            Nothing     -> return $ addHeader "*" $ addHeader "Origin, X-Requested-With, Content-Type, Accept" $ WikiText "Unknown Title"
            Just title  -> return $ addHeader "*" $ addHeader "Origin, X-Requested-With, Content-Type, Accept" title

-- Server call this method for "/random" request and return a valid wiki title
getRandom :: Handler (ResponseType WikiText)
getRandom = do
        let queries = generateQuery $ WikiParams  { formatW      = "json"
                                                  , actionW      = "query"
                                                  , titlesW      = ""
                                                  , pageW        = ""
                                                  , propW        = ""
                                                  , listW        = "random"
                                                  , sectionW     = ""
                                                  , redirectsW   = ""
                                                  , rnlimitW     = "1"
                                                  , rnnamespaceW = "0"
                                                  }
        jsonData <- liftIO (apiRequest queries)
        case getRandomWiki $ Just jsonData of
            Nothing     -> return $ addHeader "*" $ addHeader "Origin, X-Requested-With, Content-Type, Accept" $ WikiText "Unknown Title"
            Just title  -> return $ addHeader "*" $ addHeader "Origin, X-Requested-With, Content-Type, Accept" title
