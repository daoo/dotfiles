module Main (main) where

import Control.Monad (unless)
import Data.Map (Map, fromList)
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
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Util.NamedScratchpad (namedScratchpadFilterOutWorkspacePP)
import XMonad.Util.Run
import XMonad.Util.Scratchpad
import qualified XMonad.StackSet as W

-- {{{ Hooks
myManageHook :: ManageHook
myManageHook = role =? "gimp-message-dialog" --> doFloat
  where
    role = stringProperty "WM_WINDOW_ROLE"

myLayoutHook = onWorkspace "full" lfull ldef
  where
    lfull = noBorders (full ||| tall ||| long)
    ldef  = avoidStruts (borders (tall ||| long ||| full))

    full = Full
    tall = Tall 1 (3%100) (1%2)
    long = Mirror tall

    borders = lessBorders (Combine Difference Screen OnlyFloat)
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
-- }}}
-- {{{ Config
myTerminal :: String
myTerminal = "/usr/bin/urxvt"

myWorkspaces :: [WorkspaceId]
myWorkspaces = ["im", "web", "code", "code2", "term", "other", "full", "void", "music"]

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
  , ((myModKey,               xK_p),      rofiRun >>= flip safeSpawn [])
  , ((myModKey,               xK_i),      scratchpadSpawnActionTerminal myTerminal)
  , ((myModKey,               xK_Return), safeSpawnProg myTerminal)

  -- Layout
  , ((myModKey,               xK_n),     refresh)
  , ((myModKey,               xK_space), sendMessage NextLayout)
  , ((myModKey .|. shiftMask, xK_space), setLayout (Layout myLayoutHook))

  , ((myModKey, xK_comma),  sendMessage (IncMasterN (-1)))
  , ((myModKey, xK_period), sendMessage (IncMasterN 1))

  , ((myModKey, xK_at),        sendMessage Expand)
  , ((myModKey, xK_backslash), sendMessage Shrink)

  -- Tiling
  , ((myModKey, xK_t), withFocused (windows . W.sink))
  , ((myModKey, xK_u), withFocused toggleBorder)

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
  , ((myModKey,               xK_g), listWorkspaces >>= rofiPrompt "view:"  >>= createView)
  , ((myModKey,               xK_c), listWorkspaces >>= rofiPrompt "shift:" >>= createShift)

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

  -- Screen locker
  , ((myModKey, xK_b), lock)

  -- Setting keyboard layout
  , ((myModKey, xK_F1), keymap "dvpse")
  , ((myModKey, xK_F2), keymap "usaswe")

  -- Music player control
  , ((myModKey, xK_F5), playerctl "play-pause")
  , ((myModKey, xK_F6), playerctl "previous")
  , ((myModKey, xK_F7), playerctl "next")
  ]
  where
    toggleWS     = windows (W.view =<< W.tag . head . hiddenNonNSP)
    hiddenNonNSP = filter ((/= "NSP") . W.tag) . W.hidden

    createAnd "" _ = return ()
    createAnd w f = do
      s <- gets windowset
      unless (W.tagMember w s) (addHiddenWorkspace w)
      windows (f w)

    createView = (`createAnd` W.greedyView)
    createShift = (`createAnd` W.shift)

    listWorkspaces :: X [String]
    listWorkspaces = gets (map W.tag . W.workspaces . windowset)

    reload = spawn "xmonad --recompile && xmonad --restart"

    keymap name = safeSpawn "setxkbmap" [name]

    lock = safeSpawn "slock" []

    playerctl cmd = safeSpawn "playerctl" [cmd]

    rofiRun = runProcessWithInput "rofi" ["-show", "run", "-run-command", "echo -n {cmd}"] ""

    rofiPrompt prompt opts = trim <$>
      runProcessWithInput "rofi" ["-dmenu", "-p", prompt] (unlines opts)
-- }}}
-- {{{ Mouse
myMouseBindings :: Map (KeyMask, Button) (Window -> X ())
myMouseBindings = fromList
  [ ((myModKey, button1), \w -> focus w >> mouseMoveWindow w)
  , ((myModKey, button3), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)
  ]
-- }}}

main :: IO ()
main = do
  hxmobar <- spawnPipe "xmobar .xmonad/xmobarrc"

  xmonad $ ewmh $ withUrgencyHook NoUrgencyHook XConfig
    { borderWidth        = 1
    , workspaces         = myWorkspaces
    , layoutHook         = myLayoutHook
    , terminal           = myTerminal
    , normalBorderColor  = colorDarkGrey
    , focusedBorderColor = colorGreen
    , modMask            = myModKey
    , keys               = const myKeyMaps
    , logHook            = dynamicLogWithPP (namedScratchpadFilterOutWorkspacePP (myPP hxmobar))
    , startupHook        = return ()
    , mouseBindings      = const myMouseBindings
    , manageHook         = myManageHook <+> manageDocks <+> scratchpadManageHookDefault
    , handleEventHook    = fullscreenEventHook
    , focusFollowsMouse  = False
    , clickJustFocuses   = False
    }
-- vim: set foldmethod=marker:
