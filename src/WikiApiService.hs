module WikiApiService
    ( apiEndpoint
    , apiRequest
    ) where

import Network.HTTP.Client
import Network.URI                 (URI(..), URIAuth(..))
import Network.HTTP.Client.TLS     (tlsManagerSettings)
import Network.HTTP.Base           (urlEncodeVars)
import Network.HTTP.Types.Method   (methodGet)
import Network.HTTP.Types.Status   (statusCode)
import Network.HTTP.Client.Conduit (bodyReaderSource)
import Data.Conduit                (connect)
import Data.Conduit.Attoparsec     (sinkParser)
import Data.Aeson.Parser           (json)
import Data.Aeson.Types            (Value(..))
import qualified Data.ByteString.Char8 as C


-- Wikipedia API endpoint
apiEndpoint :: String
apiEndpoint = "https://en.wikipedia.org/w/api.php"

-- Create URI from queries (Not in use)
generateURI :: [(String, String)] -> URI
generateURI queries = URI { uriScheme = "https:",
                            uriAuthority = Just $ URIAuth { uriUserInfo = "",
                                                            uriRegName = "en.wikipedia.org",
                                                            uriPort = ""
                                                          },
                            uriPath = "/w/api.php?",
                            uriQuery = urlEncodeVars queries,
                            uriFragment = ""
                          }

-- Create HTTPS GET request using api endpoint and list of queries
generateRequest :: String -> [(String, String)] -> IO Request
generateRequest endpoint queries = do
  initRequest <- parseRequest endpoint
  let request = initRequest { method = methodGet
                            , secure = True
                            , queryString = C.pack $ urlEncodeVars queries
                            }
  return request
-- generateRequest queries = getRequest "https://en.wikipedia.org/w/api.php?format=json&action=parse&page=pizza&prop=text&section=0&redirects=1"

-- Send HTTPS GET request to wikipedia API and get json data
apiRequest :: [(String, String)] -> IO (Value)
apiRequest queries = do
  request <- generateRequest apiEndpoint queries
  manager <- newManager tlsManagerSettings
  jsonResponse <- let getResponse response = do
                        putStrLn $ "The status code was: " ++ show (statusCode $ responseStatus response)
                        value <- bodyReaderSource (responseBody response) `connect` sinkParser json
                        return value
                  in withResponse request manager getResponse
  -- print jsonResponse
  return jsonResponse
