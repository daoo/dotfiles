#!/usr/bin/env runhaskell
{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Main (main) where

import Control.Monad
import Data.List (sort)
import System.Directory (getDirectoryContents, removeFile, doesFileExist)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import System.Process (system)
import qualified Data.ByteString.Char8 as B

{-# INLINE ifM #-}
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM m a b = m >>= \case
  True  -> a
  False -> b

getDirectoryContents' :: FilePath -> IO [FilePath]
getDirectoryContents' = fmap (filter f) . getDirectoryContents
  where
    f "."  = False
    f ".." = False
    f _    = True

help :: String
help =
  "init - initialize state file\n\
  \remove - remove the state file\n\
  \help - this help"

stateFile :: String
stateFile = "./.episodes"

play :: FilePath -> IO ()
play = void . system . (++) "mplayer -really-quiet "

type State = [B.ByteString]

initState :: FilePath -> IO State
initState = fmap (map B.pack . sort) . getDirectoryContents'

readState :: FilePath -> IO State
readState = fmap B.lines . B.readFile

writeState :: FilePath -> State -> IO ()
writeState file = B.writeFile file . B.unlines

start :: IO State
start = do
  b <- doesFileExist stateFile
  if b
    then readState stateFile
    else hPutStrLn stderr "No state file, please initialize first." >> return []

finish :: State -> IO ()
finish [] = return ()
finish s  = writeState stateFile s

loop :: State -> IO State
loop []           = return []
loop state@(x:xs) = ifM askNoPlay (return state) $
  play (B.unpack x) >> ifM askNoContinue (return state) (loop xs)

  where
    askNoPlay = do
      B.putStr $ "Play " `B.append` x `B.append` " [Y/n]? "
      fmap (== "n") B.getLine

    askNoContinue = do
      B.putStr "Remove and continue [Y/n]? "
      fmap (== "n") getLine

main :: IO ()
main = getArgs >>= \case
  ["init"]   -> void (putStrLn "Initializing." >> initState stateFile)
  ["remove"] -> putStrLn "Removing state file." >> removeFile stateFile
  ["help"]   -> putStrLn help
  []         -> start >>= loop >>= finish
  _          -> hPutStrLn stderr help
