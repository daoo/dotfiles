module Config
  ( winBorderFocused, winBorderNormal
  , myWorkspaces
  , myXPConfig
  , myPP
  , defaultBar
  ) where

import Bar
import Data.Map (insert)
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Prompt

myWorkspaces :: [WorkspaceId]
myWorkspaces = ["im", "web", "code", "code2", "term", "other", "full", "void"]

winBorderFocused, winBorderNormal :: String
winBorderFocused = "#6dff27"
winBorderNormal  = panelBg

focusBg, focusFg, occupiedBg, occupiedFg, panelBg, panelFg, titleBg, titleFg,
  urgentBg, urgentFg, viewsBg, viewsFg, visibleBg, visibleFg :: String
focusBg    = panelBg
focusFg    = "#729fcf"
occupiedBg = panelBg
occupiedFg = "#b8b8b8"
panelBg    = "#2e3436"
panelFg    = "#b8b8b8"
titleBg    = panelBg
titleFg    = "#d3d7cf"
urgentBg   = panelBg
urgentFg   = "#ef2929"
viewsBg    = panelBg
viewsFg    = "#757575"
visibleBg  = panelBg
visibleFg  = "#ad7fa8"

panelFont :: String
panelFont = "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"

myXPConfig :: XPConfig
myXPConfig = defaultXPConfig
  { bgColor           = panelBg
  , bgHLight          = focusFg
  , fgColor           = panelFg
  , font              = panelFont
  , position          = Bottom
  , promptBorderWidth = 0

  -- Make Ctrl-C in prompt stop input
  , promptKeymap = insert (controlMask, xK_c) quit (promptKeymap defaultXPConfig)
  }

myPP :: PP
myPP = defaultPP
  { ppCurrent         = dzenColor focusFg focusBg
  , ppHidden          = dzenColor occupiedFg occupiedBg . noNSP
  , ppHiddenNoWindows = dzenColor viewsFg viewsBg . noNSP
  , ppSep             = " | "
  , ppTitle           = dzenColor titleFg titleBg
  , ppUrgent          = dzenColor urgentFg urgentBg . dzenStrip
  , ppVisible         = dzenColor visibleFg visibleBg
  , ppWsSep           = " "
  }
  where
    noNSP "NSP" = ""
    noNSP w     = w

defaultBar :: Bar
defaultBar = Bar
  { barAlign  = AlignCenter
  , barBg     = panelBg
  , barFg     = panelFg
  , barFont   = panelFont
  , barHeight = 13
  , barWidth  = 0
  , barX      = 0
  , barY      = 0
  }
