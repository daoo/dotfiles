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

data PrinterMsg = Fetched | Print String

runPrinter :: [String] -> Chan PrinterMsg -> IO ()
runPrinter [] chan = return ()
runPrinter repos chan = readChan chan >>= \case
  Print message -> putStr message >> runPrinter repos chan
  Fetched -> runPrinter (tail repos) chan

runWorker :: FilePath -> Chan PrinterMsg -> IO ()
runWorker repo printer = do
  writeChan printer $ Print syncmsg
  gitFetch repo
  commits <- gitLog "master" "origin/master" repo
  writeChan printer $ Print (syncedmsg ++ logmsg commits)
  writeChan printer Fetched
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

listGitRepos :: FilePath -> IO [String]
listGitRepos path = unixFind [path, "-type", "d", "-name", ".git", "-prune"]

main :: IO ()
main = getArgs >>= \case
  [path] -> do
    repos <- listGitRepos path
    printer <- newChan
    server <- newChan
    mapM_ (\repo -> forkIO (runWorker repo printer)) repos
    runPrinter repos printer

  _ -> do
    prog <- getProgName
    hPutStrLn stderr ("Usage: " ++ prog ++ " DIRECTORY")
    exitFailure
