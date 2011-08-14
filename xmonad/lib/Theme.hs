module Theme where

import XMonad.Prompt

winBorderFocused = "#729fcf"
winBorderNormal  = "#2e3436"
panelFg          = "#b8b8b8"
panelBg          = "#2e3436"
titleFg          = "#d3d7cf"
titleBg          = "#2e3436"
focusFg          = "#729fcf"
focusBg          = "#2e3436"
urgentFg         = "#ef2929"
urgentBg         = "#2e3436"
visibleFg        = "#ad7fa8"
visibleBg        = "#2e3436"
occupiedFg       = "#b8b8b8"
occupiedBg       = "#2e3436"
viewsFg          = "#757575"
viewsBg          = "#2e3436"

panelFont = "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"

myXPConfig :: XPConfig
myXPConfig = defaultXPConfig
  { font              = panelFont
  , bgColor           = panelBg
  , fgColor           = panelFg
  , bgHLight          = focusFg
  , position          = Top
  , promptBorderWidth = 0 }
