#!/usr/bin/env runhaskell
{-# LANGUAGE LambdaCase #-}
module Main (main) where

import Control.Concurrent
import Control.Monad
import Data.List
import System.Environment
import System.IO
import System.Process
import qualified Data.Map as M

textPurple, textReset :: String
textPurple = "\x1b[0;35m"
textReset  = "\x1b[0m"

unixFind :: [String] -> IO [String]
unixFind args = lines <$> readProcess "find" args mempty

git :: [String] -> FilePath -> IO String
git args path = readProcess "git" (showString "--git-dir=" path : args) mempty

gitFetch :: FilePath -> IO ()
gitFetch = void . git ["fetch", "--quiet"]

gitRemoteUrl :: FilePath -> IO String
gitRemoteUrl = git ["config", "--get", "remote.origin.url"]

gitLog :: String -> String -> FilePath -> IO String
gitLog rev1 rev2 = git
  [ "log"
  , showString "--format=format:" "%C(yellow)%ar%Creset %C(blue)(%ae)%Creset %s"
  , showString rev1 $ showString ".." rev2
  ]

dropWhileInclusive :: (a -> Bool) -> [a] -> [a]
dropWhileInclusive _ []     = []
dropWhileInclusive f (a:as) = if f a then dropWhileInclusive f as else as

urlHost :: String -> String
urlHost url = case dropWhileInclusive (/='@') url of
  []   -> takeWhile (/=':') url
  url' -> takeWhile (/=':') url'

indent :: String -> String
indent = unlines . map ("    " ++) . lines

data PrinterMsg   = StopPrinter | Print FilePath
data WorkerMsg    = StopWorker  | Fetch FilePath
newtype ServerMsg = Fetched String

runPrinter :: Chan PrinterMsg -> IO ()
runPrinter chan = readChan chan >>= \case
  StopPrinter -> return ()

  Print repo -> do
    putStrLn $ showString textPurple $ showString "Synced " $ showString repo textReset
    gitLog "master" "origin/master" repo >>= \case
      []  -> return ()
      new -> putStrLn "  New commits:" >> putStrLn (indent new)

    runPrinter chan

runWorker :: Chan WorkerMsg -> Chan ServerMsg -> Chan PrinterMsg -> IO ()
runWorker worker server printer = readChan worker >>= \case
  StopWorker -> return ()

  Fetch repo -> do
    gitFetch repo
    writeChan server $ Fetched repo
    writeChan printer $ Print repo
    runWorker worker server printer

spawnWorkers :: Chan ServerMsg -> Chan PrinterMsg -> [String] -> IO [Chan WorkerMsg]
spawnWorkers server printer = go M.empty
  where
    go m []           = return $ M.elems m
    go m (repo:repos) = do
      host <- urlHost `fmap` gitRemoteUrl repo
      worker <- case M.lookup host m of
        Nothing -> do
          worker <- newChan
          void . forkIO $ runWorker worker server printer
          return worker
        Just worker -> return worker

      writeChan worker $ Fetch repo
      go (M.insert host worker m) repos

waitForWorkers :: Chan ServerMsg -> [String] -> IO ()
waitForWorkers _ [] = return ()
waitForWorkers server repos = readChan server >>=
  \(Fetched repo) -> waitForWorkers server $ delete repo repos

listGitRepos :: FilePath -> IO [String]
listGitRepos path = unixFind [path, "-type", "d", "-name", ".git", "-prune"]

main :: IO ()
main = getArgs >>= \case
  [path] -> do
    repos <- listGitRepos path
    printer <- newChan
    server <- newChan
    void . forkIO $ runPrinter printer
    workers <- spawnWorkers server printer repos
    waitForWorkers server repos
    forM_ workers (`writeChan` StopWorker)
    writeChan printer StopPrinter

  _ -> do
    prog <- getProgName
    hPutStrLn stderr ("Usage: " ++ prog ++ " DIRECTORY")
