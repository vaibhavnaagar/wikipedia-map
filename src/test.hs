import Network.HTTP

main :: IO ()
main = do
 let req = getRequest "http://michael.orlitzky.com/"
 print req
 result <- simpleHTTP req
 print result
 case result of
  Left err -> do
    putStrLn "Error!"
  Right response -> do
    let (x,y,z) = rspCode response
    let hundreds = 100*x
    let tens = 10*y
    let ones = z
    let code = hundreds + tens + ones
    putStrLn $ "Response code: " ++ (show code)
 return ()
