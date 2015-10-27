#!/usr/bin/env runhaskell
{-# LANGUAGE LambdaCase #-}
module Main (main) where

import Control.Concurrent
import Control.Monad
import Data.List
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Process

textPurple, textReset :: String
textPurple = "\x1b[0;35m"
textReset  = "\x1b[0m"

indent :: String -> String
indent xs = ' ' : ' ' : xs

unixFind :: [String] -> IO [String]
unixFind args = lines <$> readProcess "find" args mempty

git :: [String] -> FilePath -> IO String
git args path = readProcess "git" (showString "--git-dir=" path : args) []

repoName :: FilePath -> String
repoName ('.':'/':path) = repoName path
repoName path = takeDirectory path

gitFetch :: FilePath -> IO ()
gitFetch = void . git ["fetch", "--quiet"]

gitFormat :: String
gitFormat = "%C(yellow)%ar%Creset %C(blue)(%ae)%Creset %s"

gitLog :: String -> String -> FilePath -> IO String
gitLog revision1 revision2 = git
  [ "log"
  , showString "--format=format:" gitFormat
  , showString revision1 $ showString ".." revision2
  ]

data PrinterMsg = StopPrinter | Print String

data ServerMsg = Fetched FilePath

runPrinter :: Chan PrinterMsg -> IO ()
runPrinter chan = readChan chan >>= \case
  StopPrinter -> return ()
  Print message -> putStr message >> runPrinter chan

runWorker :: FilePath -> Chan ServerMsg -> Chan PrinterMsg -> IO ()
runWorker repo server printer = do
  writeChan printer $ Print syncmsg
  gitFetch repo
  commits <- gitLog "master" "origin/master" repo
  writeChan printer $ Print (syncedmsg ++ logmsg commits)
  writeChan server $ Fetched repo
    where
      syncmsg :: String
      syncmsg =
        showString textPurple $
        showString "Fetching " $
        showString (repoName repo) $
        showString textReset "\n"

      syncedmsg :: String
      syncedmsg =
        showString textPurple $
        showString "Fetched " $
        showString (repoName repo) $
        showString textReset "\n"

      logmsg :: String -> String
      logmsg []      = []
      logmsg commits =
        showString (indent "New commits:\n") $
        unlines (map (indent . indent) (lines commits))

spawnWorkers :: Chan ServerMsg -> Chan PrinterMsg -> [String] -> IO ()
spawnWorkers server printer = mapM_ spawn
  where
    spawn repo = forkIO $ runWorker repo server printer

waitForWorkers :: Chan ServerMsg -> [String] -> IO ()
waitForWorkers _ []    = return ()
waitForWorkers s repos = readChan s >>=
  \(Fetched repo) -> waitForWorkers s $ delete repo repos

listGitRepos :: FilePath -> IO [String]
listGitRepos path = unixFind [path, "-type", "d", "-name", ".git", "-prune"]

main :: IO ()
main = getArgs >>= \case
  [path] -> do
    repos <- listGitRepos path
    printer <- newChan
    server <- newChan
    _ <- forkIO $ runPrinter printer
    spawnWorkers server printer repos
    waitForWorkers server repos
    writeChan printer StopPrinter

  _ -> do
    prog <- getProgName
    hPutStrLn stderr ("Usage: " ++ prog ++ " DIRECTORY")
    exitFailure
