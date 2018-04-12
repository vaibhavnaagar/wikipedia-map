module API
    ( someFunc
    ) where

import WikiAPIService (apiRequest)
import WikiParser

someFunc :: IO ()
someFunc = putStrLn "someFunc"
