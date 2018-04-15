module Main where

import ServeApi (app)
import Network.Wai.Handler.Warp

main :: IO ()
main = do
  let port = 7777
  putStrLn $ "Local server is running on port " ++ show (port)
  run port app
