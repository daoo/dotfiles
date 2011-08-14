module Theme where

import Data.Map as M

import XMonad
import XMonad.Prompt

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

panelFont = "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"

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
