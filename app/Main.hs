module Main where

import ServeApi (app)
import Network.Wai.Handler.Warp

main :: IO ()
main = run 7777 app
