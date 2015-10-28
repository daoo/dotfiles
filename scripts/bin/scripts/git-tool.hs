#!/usr/bin/env runhaskell
{-# LANGUAGE LambdaCase #-}
module Main (main) where

import Control.Concurrent
import Control.Monad
import Data.List
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.Process
import qualified Data.Map as M

dropWhileInclusive :: (a -> Bool) -> [a] -> [a]
dropWhileInclusive _ []     = []
dropWhileInclusive f (a:as) = if f a then dropWhileInclusive f as else as

urlHost :: String -> String
urlHost url = case dropWhileInclusive (/='@') url of
  []   -> takeWhile (/=':') url
  url' -> takeWhile (/=':') url'

indent :: String -> String
indent xs = (' ':' ':xs)

indents :: String -> String
indents = unlines . map (indent . indent) . lines

purple, red, reset :: String
purple = "\x1b[0;35m"
red    = "\x1b[0;31m"
reset  = "\x1b[0m"

showColor :: String -> String -> ShowS
showColor color str = showString color . showString str . showString reset

unixFind :: [String] -> IO [String]
unixFind args = lines <$> readProcess "find" args mempty

git :: [String] -> FilePath -> IO String
git args path = readProcess "git" ("-C" : path : args) mempty

gitRemoteUrl :: FilePath -> IO String
gitRemoteUrl = git ["config", "--get", "remote.origin.url"]

gitModifiedFiles :: FilePath -> IO String
gitModifiedFiles = git ["status", "-s"]

gitFetch :: FilePath -> IO ()
gitFetch = void . git ["fetch", "--quiet"]

gitLog :: String -> String -> FilePath -> IO String
gitLog rev1 rev2 = git
  [ "log"
  , showString "--format=format:" "%C(yellow)%ar%Creset %C(blue)(%ae)%Creset %s"
  , showString rev1 $ showString ".." rev2
  ]

findGitRepos :: FilePath -> IO [String]
findGitRepos path =
  unixFind [path, "-type", "d", "-name", ".git", "-prune"] >>=
  mapM (canonicalizePath . takeDirectory)

data PrinterMsg   = StopPrinter | Print String
data WorkerMsg    = StopWorker  | Fetch FilePath
newtype ServerMsg = Fetched String

runPrinter :: Chan PrinterMsg -> IO ()
runPrinter chan = readChan chan >>= \case
  StopPrinter -> return ()
  Print msg -> putStr msg >> runPrinter chan

runWorker :: Chan WorkerMsg -> Chan ServerMsg -> Chan PrinterMsg -> IO ()
runWorker worker server printer = readChan worker >>= \case
  StopWorker -> return ()
  Fetch repo -> work repo >> runWorker worker server printer
  where
    work repo = do
      gitFetch repo
      gitLog "master" "origin/master" repo >>= \case
        []      -> writeChan printer (Print (fetchedmsg repo ++ "\n"))
        commits -> writeChan printer (Print (fetchedmsg repo ++ ":\n" ++ commitsmsg commits))

      writeChan server $ Fetched repo
      runWorker worker server printer

    fetchedmsg repo = showColor purple repo mempty

    commitsmsg commits =
      showColor red (indent "new commits") $
      showString ":\n" $
      indents commits

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

fetchprog :: [FilePath] -> IO ()
fetchprog repos = do
  printer <- newChan
  server <- newChan
  void . forkIO $ runPrinter printer
  workers <- spawnWorkers server printer repos
  waitForWorkers server repos
  forM_ workers (`writeChan` StopWorker)
  writeChan printer StopPrinter

statusprog :: [FilePath] -> IO ()
statusprog = mapM_ status
  where
    status repo = do
      files   <- gitModifiedFiles repo
      commits <- gitLog "origin/master" "master" repo

      when (not (null files && null commits)) $ do
        putStr (repomsg repo)
        when (not (null files)) $ putStr (modmsg files)
        when (not (null commits)) $ putStr (commitsmsg commits)
        putStr "\n"

    repomsg repo = showColor purple repo ":\n"

    modmsg files =
      showColor red (indent "modified files") $
      showString ":\n" $
      indents files

    commitsmsg commits =
      showColor red (indent "new commits") $
      showString ":\n" $
      indents commits

main :: IO ()
main = getArgs >>= \case
  ["fetch", path] -> findGitRepos path >>= fetchprog
  ["status", path] -> findGitRepos path >>= statusprog

  _ -> do
    prog <- getProgName
    hPutStrLn stderr ("Usage: " ++ prog ++ " COMMAND DIRECTORY")
