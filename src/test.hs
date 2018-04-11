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


-- import Network.HTTP
--
-- main :: IO ()
-- main = do
--  let req = getRequest "http://michael.orlitzky.com/"
--  print req
--  result <- simpleHTTP req
--  print result
--  case result of
--   Left err -> do
--     putStrLn "Error!"
--   Right response -> do
--     let (x,y,z) = rspCode response
--     let hundreds = 100*x
--     let tens = 10*y
--     let ones = z
--     let code = hundreds + tens + ones
--     putStrLn $ "Response code: " ++ (show code)
--  return ()
