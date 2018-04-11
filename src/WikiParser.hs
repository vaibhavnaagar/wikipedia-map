module WikiParser
    (
    ) where

import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), object)


data WikiPage = Page
  { endpoint :: String,
    format :: String,
    action :: String,
    titles :: [String],
    prop :: String,
    redirects :: Int
  } deriving (Show)

-- get
-- getPageName :: [String] -> [String]
-- getPageName =
