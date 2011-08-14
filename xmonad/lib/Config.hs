module Config where

import XMonad

import Environment
import Theme

import Data.List (intersperse)

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

-- Bars
data BarAlign = AlignLeft | AlignCenter | AlignRight

instance Show BarAlign where
  show AlignLeft   = "l"
  show AlignCenter = "c"
  show AlignRight  = "r"

data Bar = Bar {
  barWidth :: Int, barHeight :: Int,
  barX :: Int, barY :: Int,
  barAlign :: BarAlign,
  barFont :: String,
  barFg :: String, barBg :: String
} deriving (Show)

barToString :: Bar -> String
barToString bar = concat $ intersperse " " bar'
  where
    bar' = [ "-fn", show $ barFont bar
           , "-fg", show $ barFg bar
           , "-bg", show $ barBg bar
           , "-x", show $ barX bar
           , "-y", show $ barY bar
           , "-w", show $ barWidth bar
           , "-h", show $ barHeight bar
           , "-ta", show $ barAlign bar ]

defaultBar :: Bar
defaultBar = Bar
  { barFont   = panelFont
  , barFg     = panelFg
  , barBg     = panelBg
  , barWidth  = 0
  , barHeight = 13
  , barX      = 0
  , barY      = 0
  , barAlign  = AlignCenter }

-- Workspaces
myWorkspaces :: [WorkspaceId]
myWorkspaces = [ "im", "web", "code", "code2", "other", "other2", "full", "void" ]

-- Modkey
myModKey :: KeyMask
myModKey = mod4Mask

