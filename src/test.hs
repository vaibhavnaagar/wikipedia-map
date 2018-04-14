#!/usr/bin/env stack
-- stack script --resolver lts-8.22
import           Data.Aeson.Parser           (json)
import           Data.Conduit                (($$), connect)
import           Data.Conduit.Attoparsec     (sinkParser)
import           Network.HTTP.Client
import           Network.HTTP.Client.Conduit (bodyReaderSource)
import           Network.HTTP.Client.TLS     (tlsManagerSettings)
import           Network.HTTP.Types.Status   (statusCode)

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings

    request <- parseRequest "https://en.wikipedia.org/w/api.php?format=json&action=parse&page=pizza&prop=text&section=0&redirects=1"

    v <- withResponse request manager $ \response -> do
        putStrLn $ "The status code was: " ++
                   show (statusCode $ responseStatus response)

        value <- bodyReaderSource (responseBody response) `connect` sinkParser json
        return value
    print v


-- format = if (formatW params) == "" then [] else [("format", formatW params)]
-- action = if (actionW params) == "" then [] else [("action", actionW params)]
-- prop = if (propW params) == "" then [] else [("prop", propW params)]
-- section = if (sectionW params) == "" then [] else [("section", sectionW params)]
-- page = if (pageW params) == "" then [] else [("page", pageW params)]
-- titles = if (titlesW params) == "" then [] else [("title", titlesW params)]
-- list = if (listW params) == "" then [] else [("list", listW params)]
-- redirects = if (redirectsW params) == "" then [] else [("redirects", redirectsW params)]
-- rnlimit = if (rnlimitW params) == "" then [] else [("rnlimit", rnlimitW params)]
-- rnnamespace = if (rnnamespaceW params) == "" then [] else [("rnnamespace", rnnamespaceW params)]
