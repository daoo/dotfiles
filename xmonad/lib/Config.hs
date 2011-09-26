module Config where

import XMonad

import Environment
import Theme

-- Software
data Software = Software {
  browser :: String,
  term :: String,
  editor :: String,
  lock :: String
} deriving (Show)

softwareDefault :: IO Software
softwareDefault = do
  b <- getEnvDefault "BROWSER" "firefox"
  e <- getEnvDefault "GUI_EDITOR" "gvim"
  t <- getEnvDefault "TERMINAL" "urxvt"
  l <- getEnvDefault "SCREENSAVER" ""
  return Software { term = t , browser = b , editor = e, lock = l }

-- Workspaces
myWorkspaces :: [WorkspaceId]
myWorkspaces = [ "im", "web", "code", "code2", "other", "other2", "full", "void" ]

-- Modkey
myModKey :: KeyMask
myModKey = mod4Mask

