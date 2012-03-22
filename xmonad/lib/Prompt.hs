module Prompt where

import System.Directory
import System.Environment

import XMonad
import XMonad.Prompt
import XMonad.Prompt.Shell hiding (shellPrompt, getCommands)

getCommands :: IO [String]
getCommands = do
    home <- getEnv "HOME"
    let d = home ++ "/.xmonad/bin"

    exists <- doesDirectoryExist d
    es <- if exists
      then getDirectoryContents d
      else return []
    return . uniqSort . filter ((/= '.') . head) $ es

shellPrompt :: XPConfig -> X ()
shellPrompt c = do
  cmds <- io getCommands
  mkXPrompt Shell c (getShellCompl cmds) spawn

