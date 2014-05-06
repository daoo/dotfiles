module Main (main) where

import Data.Map (Map, fromList, insert, union)
import Data.Ratio
import System.IO (Handle)
import XMonad
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.GridSelect
import XMonad.Actions.Navigation2D
import XMonad.Actions.NoBorders
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Reflect
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.StackSet (greedyView, view, hidden, tag, shift)
import XMonad.Util.Run
import XMonad.Util.Scratchpad

-- {{{ Bar
data BarAlign = AlignLeft | AlignCenter | AlignRight

showBarAlign :: BarAlign -> String
showBarAlign AlignCenter = "c"
showBarAlign AlignLeft   = "l"
showBarAlign AlignRight  = "r"

data Bar = Bar
  { barAlign :: BarAlign
  , barBg :: String
  , barFg :: String
  , barFont :: String
  , barHeight :: Int
  , barWidth :: Int
  , barX :: Int, barY :: Int
  }

barToString :: Bar -> String
barToString bar = showString "-fn "  $ shows (barFont bar)
                $ showString " -fg " $ shows (barFg bar)
                $ showString " -bg " $ shows (barBg bar)
                $ showString " -x "  $ shows (barX bar)
                $ showString " -y "  $ shows (barY bar)
                $ showString " -w "  $ shows (barWidth bar)
                $ showString " -h "  $ shows (barHeight bar)
                $ showString " -ta " $ showBarAlign (barAlign bar)
-- }}}
-- {{{ Hooks
myManageHook :: ManageHook
myManageHook = composeAll
  [ className =? "Skype"    --> doShift "im"
  , className =? "irc"      --> doShift "im"
  , className =? "Pidgin"   --> doShift "im"
  , className =? "Chromium" --> doShift "web"
  , className =? "Browser"  --> doShift "web"
  , className =? "Firefox"  --> doShift "web"
  , className =? "luakit"   --> doShift "web"
  , className =? "Steam"    --> doShift "other"
  , className =? "Wine"     --> doShift "full"

  , className =? "Dialog"                  --> doFloat
  , title =? "Options"                     --> doFloat
  , wmWindowRole =? "GtkFileChooserDialog" --> doFloat
  , wmWindowRole =? "Preferences"          --> doFloat
  ]
  where
    wmWindowRole = stringProperty "WM_WINDOW_ROLE"

myLayoutHook = onWorkspace "im" imLayout $ onWorkspace "full" fullLayout defaultLayout
  where
    defaultLayout = avoidStruts $ lessBorders ambiguity $ tiled ||| Mirror tiled ||| Full
    imLayout      = avoidStruts $ lessBorders ambiguity $ reflectHoriz im
    fullLayout    = noBorders $ Full ||| tiled ||| Mirror tiled

    ambiguity = Combine Difference Screen OnlyFloat
    tiled     = Tall 1 0.03 0.5
    im        = withIM (1%7) (skypeBuddyList `Or` pidgin) (Grid ||| Full)

    skypeBuddyList = Title "daoo-- - Skype\8482"
    pidgin         = ClassName "Pidgin" `And` Title "Buddy List"

myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP (myPP { ppOutput = hPutStrLn h })
-- }}}
-- {{{ Colors and fonts
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
-- }}}
-- {{{ Config
myWorkspaces :: [WorkspaceId]
myWorkspaces = ["im", "web", "code", "code2", "term", "other", "full", "void"]

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
-- }}}
-- {{{ Keys
myModKey :: KeyMask
myModKey = mod4Mask

xf86AudioLower, xf86AudioMute, xf86AudioPlay, xf86AudioRaise, xf86Email,
  xf86Favorites, xf86TouchpadToggle :: KeySym
xf86AudioLower     = 0x1008ff11
xf86AudioMute      = 0x1008ff12
xf86AudioPlay      = 0x1008ff14
xf86AudioRaise     = 0x1008ff13
xf86Email          = 0x1008ff19
xf86Favorites      = 0x1008ff30
xf86TouchpadToggle = 0x1008ffa9

dvorakMaps :: KeyMask -> (WorkspaceId -> X ()) -> [((KeyMask, KeySym), X ())]
dvorakMaps m f = zipWith (\k w -> ((m, k), f w)) workspaceKeys myWorkspaces
  where
    workspaceKeys = [ xK_ampersand, xK_bracketleft, xK_braceleft, xK_braceright
                    , xK_parenleft, xK_equal, xK_asterisk, xK_parenright
                    , xK_plus, xK_bracketright, xK_exclam ]

newKeyMaps :: Map (KeyMask, KeySym) (X ())
newKeyMaps = fromList $
  [ ((myModKey, xK_u), withFocused toggleBorder)
  , ((myModKey, xK_o), toggleWS)
  , ((myModKey, xK_r), goToSelected defaultGSConfig)

  , ((myModKey, xK_h), windowGo L False)
  , ((myModKey, xK_l), windowGo R False)
  , ((myModKey, xK_j), windowGo D False)
  , ((myModKey, xK_k), windowGo U False)
  , ((myModKey, xK_plus), sendMessage Expand)
  , ((myModKey, xK_minus), sendMessage Shrink)

  , ((myModKey .|. shiftMask, xK_g), removeEmptyWorkspace)
  , ((myModKey, xK_g), selectWorkspace myXPConfig)
  , ((myModKey, xK_c), withWorkspace myXPConfig (windows . shift))

  , ((myModKey, xK_p), launchPrompt myXPConfig)
  , ((myModKey, xK_i), scratchpadSpawnActionTerminal "/usr/bin/urxvt")
  , ((myModKey, xK_Return), safeSpawnProg "/usr/bin/urxvt")

  , ((myModKey, xK_F1), safeSpawn "/usr/bin/setxkbmap" ["dvpse"])
  , ((myModKey, xK_F2), safeSpawn "/usr/bin/setxkbmap" ["usaswe"])

  , ((myModKey, xK_x), safeSpawnProg "/usr/bin/firefox")
  , ((myModKey, xK_b), safeSpawnProg "/usr/bin/gvim")
  , ((myModKey, xK_m), safeSpawnProg "/usr/bin/zathura")
  ]

  ++ dvorakMaps myModKey (windows . greedyView)
  ++ dvorakMaps (myModKey .|. shiftMask) (windows . shift)
  where
    toggleWS = windows $ view =<< tag . head . filter ((/= "NSP") . tag) . hidden
-- }}}
-- {{{ Prompt
launchPrompt :: XPConfig -> X ()
launchPrompt c = inputPromptWithCompl c "Run" (mkComplFunFromList cmds) ?+ spawn
  where
    cmds =
      [ "SpiderOak"
      , "audacity"
      , "blender"
      , "calibre"
      , "coqide"
      , "darktable"
      , "eclipse"
      , "emacs"
      , "firefox"
      , "gimp"
      , "gvim"
      , "keepassx"
      , "libreoffice"
      , "linuxdcpp"
      , "lxappearance"
      , "minecraft"
      , "mirage"
      , "pavucontrol"
      , "pcmanfm"
      , "pidgin"
      , "skype"
      , "tuxguitar"
      , "urxvt"
      , "wirkeshark"
      , "zathura"
      ]
-- }}}

main :: IO ()
main = do
  spawn conkyCmd
  d <- spawnPipe dzenCmd

  xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
    { borderWidth        = 1
    , focusFollowsMouse  = False
    , focusedBorderColor = winBorderFocused
    , handleEventHook    = fullscreenEventHook
    , keys               = union newKeyMaps . keys defaultConfig
    , layoutHook         = myLayoutHook
    , logHook            = myLogHook d
    , manageHook         = myManageHook <+> manageDocks <+> scratchpadManageHookDefault
    , modMask            = myModKey
    , normalBorderColor  = winBorderNormal
    , terminal           = "/usr/bin/urxvt"
    , workspaces         = myWorkspaces
    }

  where
    conkyCmd = "conky -c ~/.xmonad/conkyrc 2> /dev/null | dzen2 -p " ++ barToString right
    dzenCmd  = "dzen2 -p " ++ barToString left

    (left, right) = (defaultBar, defaultBar)

-- vim: set foldmethod=marker:
