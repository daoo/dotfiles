#!/usr/bin/env runhaskell
{-# LANGUAGE LambdaCase #-}
module Main (main) where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Data.List
import System.FilePath
import System.Process
import qualified Data.Map as M

gitFormat :: String
gitFormat = "%C(yellow)%ar%Creset %C(blue)(%ae)%Creset %s"

textPurple, textReset :: String
textPurple = "\x1b[0;35m"
textReset  = "\x1b[0m"

git :: FilePath -> [String] -> IO String
git dir args = readProcess "git"
  (showString "--git-dir=" dir : args)
  []

gitFetch :: FilePath -> IO String
gitFetch = (`git` [ "fetch", "--quiet" ])

gitRemoteUrl :: FilePath -> IO String
gitRemoteUrl = (`git` [ "config", "--get", "remote.origin.url" ])

gitLog :: FilePath -> String -> String -> IO String
gitLog dir from to = git dir
  [ "log"
  , showString "--format=format:" gitFormat
  , showString from $ showString ".." to
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

data PrinterMsg   = StopPrinter | Print String FilePath
data WorkerMsg    = StopWorker  | Fetch String FilePath
newtype ServerMsg = Fetched String

printer :: Chan PrinterMsg -> IO ()
printer chan = readChan chan >>= \case
  StopPrinter -> return ()

  Print repo dir -> do
    putStrLn $ showString textPurple $ showString "Synced " $ showString repo textReset
    gitLog dir "master" "origin/master" >>= \case
      []  -> return ()
      new -> putStrLn "  New commits:" >> putStrLn (indent new)

    printer chan

worker :: Chan WorkerMsg -> Chan ServerMsg -> Chan PrinterMsg -> IO ()
worker w s p = readChan w >>= \case
  StopWorker -> return ()

  Fetch repo dir -> do
    gitFetch dir
    writeChan s $ Fetched repo
    writeChan p $ Print repo dir
    worker w s p

spawnWorkers :: Chan ServerMsg -> Chan PrinterMsg -> [String] -> IO [Chan WorkerMsg]
spawnWorkers s p = go M.empty
  where
    go m []           = return $ M.elems m
    go m (repo:repos) = do
      let dir = repo </> ".git"
      host <- urlHost `fmap` gitRemoteUrl dir
      w <- case M.lookup host m of
        Nothing -> do
          w <- newChan
          forkIO $ worker w s p
          return w
        Just w -> return w

      writeChan w $ Fetch repo dir
      go (M.insert host w m) repos

waitForWorkers :: Chan ServerMsg -> [String] -> IO ()
waitForWorkers _ []    = return ()
waitForWorkers s repos = readChan s >>=
  \(Fetched repo) -> waitForWorkers s $ delete repo repos

main :: IO ()
main = do
  repos <- lines `fmap` readFile ".git-repos"
  p <- newChan
  s <- newChan
  forkIO $ printer p
  ws <- spawnWorkers s p repos
  waitForWorkers s repos
  forM_ ws (`writeChan` StopWorker)
  writeChan p StopPrinter
