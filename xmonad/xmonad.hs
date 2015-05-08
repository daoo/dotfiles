module Main (main) where

import BinarySpacePartition
import Data.Map (Map, fromList, insert)
import Data.Ratio ((%))
import System.Exit (exitSuccess)
import System.IO (Handle)
import XMonad hiding (Color)
import XMonad.Actions.DynamicWorkspaces
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
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.Scratchpad
import qualified XMonad.StackSet as W

-- {{{ Hooks
myManageHook :: ManageHook
myManageHook = composeAll
  [ className =? "Skype"       --> doShift "im"
  , className =? "irc"         --> doShift "im"
  , className =? "Pidgin"      --> doShift "im"
  , className =? "Chromium"    --> doShift "web"
  , className =? "Browser"     --> doShift "web"
  , className =? "Firefox"     --> doShift "web"
  , className =? "luakit"      --> doShift "web"
  , className =? "claws-mail"  --> doShift "mail"
  , className =? "Steam"       --> doShift "other"
  , className =? "Wine"        --> doShift "full"

  , className =? "Dialog"                  --> doFloat
  , title =? "Options"                     --> doFloat
  , wmWindowRole =? "GtkFileChooserDialog" --> doFloat
  , wmWindowRole =? "Preferences"          --> doFloat
  ]
  where
    wmWindowRole = stringProperty "WM_WINDOW_ROLE"

myLayoutHook = onWorkspace "im" imLayout $ onWorkspace "full" fullLayout defaultLayout
  where
    defaultLayout = avoidStruts $ lessBorders ambiguity $ emptyBSP ||| Full
    imLayout      = avoidStruts $ lessBorders ambiguity $ reflectHoriz im
    fullLayout    = noBorders $ Full ||| emptyBSP

    ambiguity = Combine Difference Screen OnlyFloat
    im        = withIM (1%7) (skypeBuddyList `Or` pidgin) (Grid ||| Full)

    skypeBuddyList = Title "daoo-- - Skype\8482"
    pidgin         = ClassName "Pidgin" `And` Title "Buddy List"
-- }}}
-- {{{ Colors and fonts
colorRed, colorGreen, colorBlue, colorPurple :: String
colorRed    = "#ef2929"
colorGreen  = "#6dff27"
colorBlue   = "#729fcf"
colorPurple = "#ad7fa8"

colorDarkGrey, colorLightGrey, colorGrey :: String
colorDarkGrey  = "#2e3436"
colorLightGrey = "#b8b8b8"
colorGrey      = "#757575"

panelFont :: String
panelFont = "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"
-- }}}
-- {{{ Config
myTerminal :: String
myTerminal = "/usr/bin/urxvt"

myWorkspaces :: [WorkspaceId]
myWorkspaces = ["im", "web", "code", "code2", "term", "other", "full", "void", "mail"]

myXPConfig :: XPConfig
myXPConfig = defaultXPConfig
  { bgColor           = colorDarkGrey
  , bgHLight          = colorBlue
  , fgColor           = colorLightGrey
  , font              = panelFont
  , position          = Bottom
  , promptBorderWidth = 0

  -- Make Ctrl-C in prompt stop input
  , promptKeymap = insert (controlMask, xK_c) quit (promptKeymap defaultXPConfig)
  }

myPP :: Handle -> PP
myPP handle = defaultPP
  { ppCurrent         = xmobarColor colorBlue      colorDarkGrey
  , ppHidden          = xmobarColor colorLightGrey colorDarkGrey
  , ppHiddenNoWindows = xmobarColor colorGrey      colorDarkGrey
  , ppSep             = " | "
  , ppTitle           = xmobarColor colorLightGrey colorDarkGrey
  , ppUrgent          = xmobarColor colorRed       colorDarkGrey
  , ppVisible         = xmobarColor colorPurple    colorDarkGrey
  , ppWsSep           = " "

  , ppOutput = hPutStrLn handle
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

myKeyMaps :: Map (KeyMask, KeySym) (X ())
myKeyMaps = fromList
  -- Launching and killing programs
  [ ((myModKey .|. shiftMask, xK_c),      kill)
  , ((myModKey,               xK_p),      launchPrompt myXPConfig)
  , ((myModKey,               xK_i),      scratchpadSpawnActionTerminal myTerminal)
  , ((myModKey,               xK_Return), safeSpawnProg myTerminal)

  -- Layout
  , ((myModKey,               xK_n),     refresh)
  , ((myModKey,               xK_at),    sendMessage Expand)
  , ((myModKey,               xK_minus), sendMessage Shrink)
  , ((myModKey,               xK_space), sendMessage NextLayout)
  , ((myModKey .|. shiftMask, xK_space), setLayout (Layout myLayoutHook))

  -- Tiling
  , ((myModKey, xK_t), withFocused $ windows . W.sink)
  , ((myModKey, xK_u), withFocused toggleBorder)

  , ((myModKey .|. altMask,              xK_l), sendMessage $ ExpandTowards R)
  , ((myModKey .|. altMask,              xK_h), sendMessage $ ExpandTowards L)
  , ((myModKey .|. altMask,              xK_j), sendMessage $ ExpandTowards D)
  , ((myModKey .|. altMask,              xK_k), sendMessage $ ExpandTowards U)
  , ((myModKey .|. altMask .|. ctrlMask, xK_l), sendMessage $ ShrinkFrom R)
  , ((myModKey .|. altMask .|. ctrlMask, xK_h), sendMessage $ ShrinkFrom L)
  , ((myModKey .|. altMask .|. ctrlMask, xK_j), sendMessage $ ShrinkFrom D)
  , ((myModKey .|. altMask .|. ctrlMask, xK_k), sendMessage $ ShrinkFrom U)
  , ((myModKey,                          xK_r), sendMessage Rotate)
  , ((myModKey,                          xK_s), sendMessage Swap)

  -- Focus and swapping
  , ((myModKey,               xK_Tab), windows W.focusDown)
  , ((myModKey .|. shiftMask, xK_Tab), windows W.focusUp)
  , ((myModKey,               xK_m),   windows W.focusMaster)
  , ((myModKey .|. shiftMask, xK_m),   windows W.swapMaster)

  , ((myModKey,               xK_h), windowGo L False)
  , ((myModKey,               xK_l), windowGo R False)
  , ((myModKey,               xK_j), windowGo D False)
  , ((myModKey,               xK_k), windowGo U False)
  , ((myModKey .|. shiftMask, xK_h), windowSwap L False)
  , ((myModKey .|. shiftMask, xK_l), windowSwap R False)
  , ((myModKey .|. shiftMask, xK_j), windowSwap D False)
  , ((myModKey .|. shiftMask, xK_k), windowSwap U False)

  -- Multiple screens
  , ((myModKey,               xK_w), screenWorkspace 0 >>= flip whenJust (windows . W.view))
  , ((myModKey,               xK_e), screenWorkspace 1 >>= flip whenJust (windows . W.view))
  , ((myModKey .|. shiftMask, xK_w), screenWorkspace 0 >>= flip whenJust (windows . W.shift))
  , ((myModKey .|. shiftMask, xK_e), screenWorkspace 1 >>= flip whenJust (windows . W.shift))

  -- Handling workspaces
  , ((myModKey,               xK_o), toggleWS)
  , ((myModKey .|. shiftMask, xK_g), removeEmptyWorkspace)
  , ((myModKey,               xK_g), selectWorkspace myXPConfig)
  , ((myModKey,               xK_c), withWorkspace myXPConfig (windows . W.shift))

  -- Workspace keys
  , ((myModKey,               xK_ampersand),   windows (W.greedyView (myWorkspaces !! 0)))
  , ((myModKey,               xK_bracketleft), windows (W.greedyView (myWorkspaces !! 1)))
  , ((myModKey,               xK_braceleft),   windows (W.greedyView (myWorkspaces !! 2)))
  , ((myModKey,               xK_braceright),  windows (W.greedyView (myWorkspaces !! 3)))
  , ((myModKey,               xK_parenleft),   windows (W.greedyView (myWorkspaces !! 4)))
  , ((myModKey,               xK_equal),       windows (W.greedyView (myWorkspaces !! 5)))
  , ((myModKey,               xK_asterisk),    windows (W.greedyView (myWorkspaces !! 6)))
  , ((myModKey,               xK_parenright),  windows (W.greedyView (myWorkspaces !! 7)))
  , ((myModKey,               xK_plus),        windows (W.greedyView (myWorkspaces !! 8)))
  , ((myModKey .|. shiftMask, xK_ampersand),   windows (W.shift (myWorkspaces !! 0)))
  , ((myModKey .|. shiftMask, xK_bracketleft), windows (W.shift (myWorkspaces !! 1)))
  , ((myModKey .|. shiftMask, xK_braceleft),   windows (W.shift (myWorkspaces !! 2)))
  , ((myModKey .|. shiftMask, xK_braceright),  windows (W.shift (myWorkspaces !! 3)))
  , ((myModKey .|. shiftMask, xK_parenleft),   windows (W.shift (myWorkspaces !! 4)))
  , ((myModKey .|. shiftMask, xK_equal),       windows (W.shift (myWorkspaces !! 5)))
  , ((myModKey .|. shiftMask, xK_asterisk),    windows (W.shift (myWorkspaces !! 6)))
  , ((myModKey .|. shiftMask, xK_parenright),  windows (W.shift (myWorkspaces !! 7)))
  , ((myModKey .|. shiftMask, xK_plus),        windows (W.shift (myWorkspaces !! 8)))

  -- Restarting and stopping xmonad
  , ((myModKey .|. shiftMask, xK_q), io exitSuccess)
  , ((myModKey,               xK_q), reload)

  , ((myModKey,               xK_b), lock)

  -- Setting keyboard layout
  , ((myModKey, xK_F1), keymap "dvpse")
  , ((myModKey, xK_F2), keymap "usaswe")
  ]
  where
    altMask  = mod1Mask
    ctrlMask = controlMask

    toggleWS     = windows $ W.view =<< W.tag . head . hiddenNonNSP
    hiddenNonNSP = filter ((/= "NSP") . W.tag) . W.hidden

    reload = do
      safeSpawn "xmonad" ["--recompile"]
      safeSpawn "xmonad" ["--restart"]

    keymap name = safeSpawn "setxkbmap" [name]

    lock = safeSpawn "i3lock-fancy" []
-- }}}
-- {{{ Prompt
launchPrompt :: XPConfig -> X ()
launchPrompt c = inputPromptWithCompl c "Run" (mkComplFunFromList cmds) ?+ spawn
  where
    cmds =
      [ "audacity"
      , "blender"
      , "chromium"
      , "emacs"
      , "firefox"
      , "gimp"
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
      , "termite"
      , "tuxguitar"
      , "urxvt"
      , "wirkeshark"
      , "zathura"
      ]
-- }}}

main :: IO ()
main = do
  hxmobar <- spawnPipe "xmobar .xmonad/xmobarrc"

  xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
    { borderWidth        = 1
    , focusFollowsMouse  = False
    , focusedBorderColor = colorGreen
    , handleEventHook    = fullscreenEventHook
    , keys               = const myKeyMaps
    , layoutHook         = myLayoutHook
    , logHook            = dynamicLogWithPP $ namedScratchpadFilterOutWorkspacePP $ myPP hxmobar
    , manageHook         = myManageHook <+> manageDocks <+> scratchpadManageHookDefault
    , modMask            = myModKey
    , normalBorderColor  = colorDarkGrey
    , terminal           = myTerminal
    , workspaces         = myWorkspaces
    }
-- vim: set foldmethod=marker:
