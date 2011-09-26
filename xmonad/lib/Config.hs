module Config where

import Data.Map as M

import Bar
import Environment
import Software

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Prompt

-- Software
softwareDefault :: IO Software
softwareDefault = do
  b <- getEnvDefault "BROWSER" "firefox"
  e <- getEnvDefault "GUI_EDITOR" "gvim"
  t <- getEnvDefault "TERMINAL" "urxvt"
  l <- getEnvDefault "SCREENSAVER" ""
  return Software { term = t , browser = b , editor = e, lock = l }

-- Workspaces
myWorkspaces :: [WorkspaceId]
myWorkspaces = ["im", "web", "code", "code2", "other", "other2", "full", "void"]

-- Modkey
myModKey :: KeyMask
myModKey = mod4Mask

-- Colors
winBorderFocused = focusFg
winBorderNormal  = panelBg

panelFg    = "#b8b8b8"
panelBg    = "#2e3436"
titleFg    = "#d3d7cf"
titleBg    = panelBg
focusFg    = "#729fcf"
focusBg    = panelBg
urgentFg   = "#ef2929"
urgentBg   = panelBg
visibleFg  = "#ad7fa8"
visibleBg  = panelBg
occupiedFg = "#b8b8b8"
occupiedBg = panelBg
viewsFg    = "#757575"
viewsBg    = panelBg

-- Fonts
panelFont = "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"

-- XPConfig
myXPConfig :: XPConfig
myXPConfig = defaultXPConfig
  { font              = panelFont
  , bgColor           = panelBg
  , fgColor           = panelFg
  , bgHLight          = focusFg
  , position          = Bottom
  , promptBorderWidth = 0

  -- Make Ctrl-C in prompt stop input
  , promptKeymap = M.fromList [((controlMask,xK_c), quit)] `M.union` promptKeymap defaultXPConfig }

-- Log Hook
myPP :: PP
myPP = defaultPP
  { ppUrgent          = color urgentFg urgentBg . dzenStrip
  , ppCurrent         = color focusFg focusBg
  , ppVisible         = color visibleFg visibleBg
  , ppHidden          = color occupiedFg occupiedBg . noNSP
  , ppHiddenNoWindows = color viewsFg viewsBg . noNSP
  , ppTitle           = color titleFg titleBg
  , ppWsSep           = " "
  , ppSep             = " | " }
  where
    color     = dzenColor
    noNSP ws  = if (ws == "NSP") then "" else ws

-- Default Bar
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
