module Main where

import System.FilePath

import Data.Char
import Data.List

data Language = C | CPP
data File = Header | Source

findTypeName :: [String] -> Maybe String
findTypeName lines = case filter (isInfixOf "class") lines of
  []       -> Nothing
  (line:_) -> case words line of
    (_:t:_) -> Just t
    _       -> Nothing

makeCPPGuardName :: FilePath -> String -> String
makeCPPGuardName d t = concat [makeGuardPrefix d, "_", t', "_HPP_"]
  where
    t' = map toUpper t

makeCGuardName :: FilePath -> String
makeCGuardName d = concat [(makeGuardPrefix . takeDirectory) d, "_H_"]

makeGuardPrefix :: FilePath -> String
makeGuardPrefix = (map f) . dropTrailingPathSeparator
  where
    f '/' = '_'
    f c   = toUpper c

makeGuard :: String -> ([String], String)
makeGuard name = ( ["#ifndef " ++ name, "#define " ++ name]
                 , "#endif // " ++ name)

main :: IO ()
main = do
  putStrLn "TODO"
