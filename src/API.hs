module API
    ( someFunc
    ) where

import WikiApiService (apiRequest)
import WikiParser

someFunc :: IO ()
someFunc = putStrLn "someFunc"
