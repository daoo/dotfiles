module Main (main) where

import Control.Monad (unless)
import Data.Map (Map, fromList)
import Data.Monoid ((<>))
import Data.Ratio ((%))
import System.Exit (exitSuccess)
import System.IO (Handle, hPutStrLn)
import XMonad
import XMonad.Actions.DynamicWorkspaces (addHiddenWorkspace, removeEmptyWorkspace)
import XMonad.Actions.Navigation2D (windowGo, windowSwap, Direction2D(..))
import XMonad.Actions.NoBorders (toggleBorder)
import XMonad.Actions.WindowBringer (windowMap)
import XMonad.Hooks.DynamicLog (PP(..), defaultPP, dynamicLogWithPP, xmobarColor, trim)
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks)
import XMonad.Hooks.UrgencyHook (NoUrgencyHook(NoUrgencyHook), withUrgencyHook)
import XMonad.Layout.NoBorders (lessBorders, Ambiguity(..), With(..))
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Util.NamedScratchpad (namedScratchpadFilterOutWorkspacePP)
import XMonad.Util.Run (spawnPipe, safeSpawn, safeSpawnProg, runProcessWithInput)
import XMonad.Util.Scratchpad (scratchpadSpawnActionCustom, scratchpadManageHookDefault)
import qualified Data.Map as M
import qualified XMonad.StackSet as W

myManageHook :: ManageHook
myManageHook = role =? "gimp-message-dialog" --> doFloat
  where
    role = stringProperty "WM_WINDOW_ROLE"

myLayoutHook = onWorkspace "full" lfull ldef
  where
    lfull = lessBorders borders (full ||| tall ||| wide)
    ldef  = avoidStruts (lessBorders borders (tall ||| wide ||| full))

    full = Full
    tall = Tall 1 (3%100) (1%2)
    wide = Mirror tall

    borders = Combine Difference Screen OnlyFloat

colorRed, colorGreen, colorBlue, colorPurple :: String
colorRed    = "#ef2929"
colorGreen  = "#6dff27"
colorBlue   = "#729fcf"
colorPurple = "#ad7fa8"

colorDarkGrey, colorLightGrey, colorGrey :: String
colorDarkGrey  = "#2e3436"
colorLightGrey = "#b8b8b8"
colorGrey      = "#757575"

myTerminal :: String
myTerminal = "/usr/bin/st"

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
  [ xK_c      ! kill
  , xK_p      # rofiRun
  , xK_i      # scratchpadSpawnActionCustom (myTerminal ++ " -c scratchpad")
  , xK_Return # safeSpawnProg myTerminal

  -- Layout
  , xK_n     # refresh
  , xK_space # sendMessage NextLayout
  , xK_space ! setLayout (Layout myLayoutHook)

  , xK_comma  # sendMessage (IncMasterN (-1))
  , xK_period # sendMessage (IncMasterN 1)

  , xK_at        # sendMessage Expand
  , xK_backslash # sendMessage Shrink

  -- Tiling
  , xK_t # withFocused (windows . W.sink)
  , xK_u # withFocused toggleBorder

  -- Focus and swapping
  , xK_f # selectWindow

  , xK_Tab # windows W.focusDown
  , xK_Tab ! windows W.focusUp
  , xK_m   # windows W.focusMaster
  , xK_m   ! windows W.swapMaster

  , xK_h # windowGo L False
  , xK_l # windowGo R False
  , xK_j # windowGo D False
  , xK_k # windowGo U False
  , xK_h ! windowSwap L False
  , xK_l ! windowSwap R False
  , xK_j ! windowSwap D False
  , xK_k ! windowSwap U False

  -- Multiple screens
  , xK_w # screenWorkspace 0 >>= flip whenJust (windows . W.view)
  , xK_e # screenWorkspace 1 >>= flip whenJust (windows . W.view)
  , xK_w ! screenWorkspace 0 >>= flip whenJust (windows . W.shift)
  , xK_e ! screenWorkspace 1 >>= flip whenJust (windows . W.shift)

  -- Handling workspaces
  , xK_o # toggleWS
  , xK_r ! removeEmptyWorkspace
  , xK_v # listWorkspaces >>= rofiPrompt "view:"  >>= createView
  , xK_s # listWorkspaces >>= rofiPrompt "shift:" >>= createShift

  -- Workspace keys
  , xK_ampersand   # windows (W.greedyView (myWorkspaces !! 0))
  , xK_bracketleft # windows (W.greedyView (myWorkspaces !! 1))
  , xK_braceleft   # windows (W.greedyView (myWorkspaces !! 2))
  , xK_braceright  # windows (W.greedyView (myWorkspaces !! 3))
  , xK_parenleft   # windows (W.greedyView (myWorkspaces !! 4))
  , xK_equal       # windows (W.greedyView (myWorkspaces !! 5))
  , xK_asterisk    # windows (W.greedyView (myWorkspaces !! 6))
  , xK_parenright  # windows (W.greedyView (myWorkspaces !! 7))
  , xK_plus        # windows (W.greedyView (myWorkspaces !! 8))
  , xK_ampersand   ! windows (W.shift (myWorkspaces !! 0))
  , xK_bracketleft ! windows (W.shift (myWorkspaces !! 1))
  , xK_braceleft   ! windows (W.shift (myWorkspaces !! 2))
  , xK_braceright  ! windows (W.shift (myWorkspaces !! 3))
  , xK_parenleft   ! windows (W.shift (myWorkspaces !! 4))
  , xK_equal       ! windows (W.shift (myWorkspaces !! 5))
  , xK_asterisk    ! windows (W.shift (myWorkspaces !! 6))
  , xK_parenright  ! windows (W.shift (myWorkspaces !! 7))
  , xK_plus        ! windows (W.shift (myWorkspaces !! 8))

  -- Restarting and stopping xmonad
  , xK_q # reload
  , xK_q ! io exitSuccess

  -- Screen locker
  , xK_b # lock

  -- Setting keyboard layout
  , xK_F1 # keymap "dvpse"
  , xK_F2 # keymap "usaswe"

  -- Music player control
  , xK_F5 # playerctl "play-pause"
  , xK_F6 # playerctl "previous"
  , xK_F7 # playerctl "next"
  ]
  where
    key # action = ((myModKey, key), action)
    key ! action = ((myModKey .|. shiftMask, key), action)

    infixr 0 #
    infixr 0 !

    toggleWS     = windows (W.view =<< W.tag . head . hiddenNonNSP)
    hiddenNonNSP = filter ((/= "NSP") . W.tag) . W.hidden

    createAnd newtag f = unless (null newtag) $
      addHiddenWorkspace newtag >> windows (f newtag)

    createView = (`createAnd` W.greedyView)
    createShift = (`createAnd` W.shift)

    listWorkspaces = gets (map W.tag . W.workspaces . windowset)

    selectWindow = do
      m <- windowMap
      rofiPrompt "focus:" (M.keys m) >>= \k -> unless (null k) $
        maybe (return ()) (windows . W.focusWindow) (M.lookup k m)

    reload = spawn "xmonad --recompile && xmonad --restart"

    keymap name = safeSpawn "setxkbmap" [name]

    lock = safeSpawn "physlock" []

    playerctl cmd = safeSpawn "playerctl" [cmd]

    rofi = runProcessWithInput "rofi"

    rofiRun = rofi ["-show", "run", "-run-command", "echo -n {cmd}"] "" >>= flip safeSpawn []

    rofiPrompt prompt opts = trim <$> rofi ["-dmenu", "-p", prompt] (unlines opts)

myMouseBindings :: Map (KeyMask, Button) (Window -> X ())
myMouseBindings = fromList
  [ ((myModKey, button1), \w -> focus w >> mouseMoveWindow w)
  , ((myModKey, button3), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)
  ]

main :: IO ()
main = do
  hxmobar <- spawnPipe "xmobar .xmonad/xmobarrc"

  xmonad $ ewmh $ withUrgencyHook NoUrgencyHook def
    { borderWidth        = 1
    , workspaces         = myWorkspaces
    , layoutHook         = myLayoutHook
    , terminal           = myTerminal
    , normalBorderColor  = colorDarkGrey
    , focusedBorderColor = colorGreen
    , modMask            = myModKey
    , keys               = const myKeyMaps
    , logHook            = dynamicLogWithPP (namedScratchpadFilterOutWorkspacePP (myPP hxmobar))
    , manageHook         = myManageHook <> scratchpadManageHookDefault <> manageDocks
    , handleEventHook    = fullscreenEventHook <+> docksEventHook
    , focusFollowsMouse  = False
    , clickJustFocuses   = False
    }
-- vim: set foldmethod=marker:
