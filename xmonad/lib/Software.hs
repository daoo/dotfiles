module Software where

data Software = Software {
  browser :: String,
  term :: String,
  editor :: String,
  lock :: String
} deriving (Show)

