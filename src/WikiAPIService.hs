module WikiAPIService
    (
    ) where


import Control.Monad (void)
import Network.URI (URI(..), URIAuth(..))
import Network.HTTP (simpleHTTP, getRequest)
import Network.HTTP.Base (rspBody, urlEncodeVars, Request(..), RequestMethod(..))
import Network.HTTP.Headers (mkHeader, Header, HeaderName(..))


generateURI :: [(String, String)] -> URI
generateURI queries = URI { uriScheme = "http:",
                            uriAuthority = Just $ URIAuth { uriUserInfo = "",
                                                            uriRegName = "en.wikipedia.org",
                                                            uriPort = ""
                                                          },
                            uriPath = "/w/api.php?",
                            uriQuery = urlEncodeVars queries,
                            uriFragment = ""
                          }


generateRequest :: [(String, String)] -> Request String
-- generateRequest queries = Request { rqURI = generateURI queries,
--                                     rqMethod = GET,
--                                     rqHeaders = [mkHeader HdrContentLength "0", mkHeader HdrUserAgent "wikipedia-map/0.1"],
--                                     rqBody = ""
--                                   }
generateRequest queries = getRequest "http://en.wikipedia.org/w/api.php?format=json&action=parse&page=pizza&prop=text&section=0&redirects=1"

-- rqHeaders = [mkHeader HdrUserAgent "wikipedia-map/0.1"],


apiRequest :: [(String, String)] -> IO (Maybe String)
apiRequest queries = do
  let request = generateRequest queries
  print request
  result <- simpleHTTP request
  print result
  case result of
    Left _ -> return Nothing
    Right response -> return $ Just (rspBody response)


-- sections :: IO (Maybe [String])
-- sections pg = do
--   let queries = [ ("format", "json")
--                 , ("action", "parse")
--                 , ("prop", "sections")
--                 , ("page", "pizza")
--                 ]
--   maybeResults <- apiRequest queries
--   return $ fmap (extractAllAttrValues "line") maybeResults

-- wikipedia API endpoint
wikipediaAPI :: String
wikipediaAPI = "https://en.wikipedia.org/w/api.php"
