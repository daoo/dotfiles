module Main (main) where

import Data.Map (Map, fromList)
import Data.Ratio ((%))
import System.Exit (exitSuccess)
import System.IO (Handle, hPutStrLn)
import XMonad
import XMonad.Actions.Navigation2D (windowGo, windowSwap, Direction2D(..))
import XMonad.Actions.NoBorders (toggleBorder)
import XMonad.Hooks.DynamicLog (PP(..), dynamicLogWithPP, xmobarColor, wrap)
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(ToggleStruts))
import XMonad.Hooks.ManageHelpers (doCenterFloat)
import XMonad.Hooks.UrgencyHook (NoUrgencyHook(NoUrgencyHook), withUrgencyHook)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Util.Run (spawnPipe, safeSpawn, safeSpawnProg)
import qualified XMonad.StackSet as W
import qualified XMonad.Util.NamedScratchpad as NS

scratchpad :: NS.NamedScratchpad
scratchpad = NS.NS "alacritty" "alacritty --class=scratchpad" query hook
  where
    query = appName =? "scratchpad"
    hook = NS.customFloating (W.RationalRect 0.25 0.375 0.5 0.25)

myManageHook :: ManageHook
myManageHook =
  (isNextcloud --> doShift "1") <>
  (isFirefox --> doShift "2") <>
  (isSteam --> doShift "6") <>
  (isGimpDialog --> doFloat) <>
  (isGnomeKeyring --> doCenterFloat) <>
  NS.namedScratchpadManageHook [scratchpad] <>
  manageDocks
  where
    isNextcloud = className =? "Nextcloud"
    isFirefox = appName =? "Navigator"
    isGimpDialog = className =? "Gimp" <&&> not <$> windowRole =? "gimp-image-window"
    isSteam = className =? "Steam"
    isGnomeKeyring = className =? "Gcr-prompter"

    windowRole = stringProperty "WM_WINDOW_ROLE"

myLayoutHook = avoidStruts (smartBorders (tall ||| Mirror tall ||| Full))
  where
    tall = Tall 1 (3%100) (1%2)

colorRed, colorGreen, colorBlue, colorPurple :: String
colorRed    = "#cc241d"
colorGreen  = "#b8bb26"
colorBlue   = "#458588"
colorPurple = "#b16286"

colorBackground, colorForeground, colorForegroundDark :: String
colorBackground     = "#1d2021"
colorForeground     = "#fbf1c7"
colorForegroundDark = "#757575"

myWorkspaces :: [WorkspaceId]
myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "NSP"]

myPP :: Handle -> PP
myPP handle = NS.namedScratchpadFilterOutWorkspacePP def
  { ppCurrent = xmobarColor colorForeground colorBlue . wrapWS
  , ppHiddenNoWindows = xmobarColor colorForegroundDark colorBackground . wrapWS
  , ppUrgent = xmobarColor colorForeground colorRed . wrapWS
  , ppVisible = xmobarColor colorForeground colorPurple . wrapWS
  , ppHidden = wrapWS
  , ppTitle = wrap " " ""
  , ppLayout = wrapWS
  , ppSep = "|"
  , ppWsSep = ""
  , ppOutput = hPutStrLn handle
  }
  where
    wrapWS = wrap " " " "

myModKey :: KeyMask
myModKey = mod4Mask

xf86AudioLower, xf86AudioMute, xf86AudioRaise :: KeySym
xf86AudioLower = 0x1008ff11
xf86AudioMute  = 0x1008ff12
xf86AudioRaise = 0x1008ff13

myKeyMaps :: Map (KeyMask, KeySym) (X ())
myKeyMaps = fromList
  -- Launching and killing programs
  [ xK_c      ! kill
  , xK_p      # safeSpawnProg "kupfer"
  , xK_i      # NS.namedScratchpadAction [scratchpad] (NS.name scratchpad)
  , xK_Return # safeSpawnProg "alacritty"

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
  , xK_x # sendMessage ToggleStruts

  -- Focus and swapping
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

  -- Workspace toggling
  , xK_o # toggleWorkspace

  -- Workspace keys
  , xK_ampersand    # windows (W.greedyView (myWorkspaces !! 0))
  , xK_bracketleft  # windows (W.greedyView (myWorkspaces !! 1))
  , xK_braceleft    # windows (W.greedyView (myWorkspaces !! 2))
  , xK_braceright   # windows (W.greedyView (myWorkspaces !! 3))
  , xK_parenleft    # windows (W.greedyView (myWorkspaces !! 4))
  , xK_equal        # windows (W.greedyView (myWorkspaces !! 5))
  , xK_asterisk     # windows (W.greedyView (myWorkspaces !! 6))
  , xK_parenright   # windows (W.greedyView (myWorkspaces !! 7))
  , xK_plus         # windows (W.greedyView (myWorkspaces !! 8))
  , xK_bracketright # windows (W.greedyView (myWorkspaces !! 9))
  , xK_ampersand    ! windows (W.shift (myWorkspaces !! 0))
  , xK_bracketleft  ! windows (W.shift (myWorkspaces !! 1))
  , xK_braceleft    ! windows (W.shift (myWorkspaces !! 2))
  , xK_braceright   ! windows (W.shift (myWorkspaces !! 3))
  , xK_parenleft    ! windows (W.shift (myWorkspaces !! 4))
  , xK_equal        ! windows (W.shift (myWorkspaces !! 5))
  , xK_asterisk     ! windows (W.shift (myWorkspaces !! 6))
  , xK_parenright   ! windows (W.shift (myWorkspaces !! 7))
  , xK_plus         ! windows (W.shift (myWorkspaces !! 8))
  , xK_bracketright ! windows (W.shift (myWorkspaces !! 9))

  -- Restarting and stopping xmonad
  , xK_q # spawn "xmonad --recompile && xmonad --restart"
  , xK_q ! io exitSuccess

  -- Screen locker
  , xK_b # safeSpawn "physlock" ["-m"]

  -- Setting keyboard layout
  -- Note: z and apostrophe is the same physical key in both layouts
  , xK_z          # keymap "dvoormak"
  , xK_apostrophe # keymap "usaswe"

  -- Music player control
  , xK_F5 # playerctl "play-pause"
  , xK_F6 # playerctl "previous"
  , xK_F7 # playerctl "next"

  -- Volume control
  , xf86AudioMute  & volumectl ["toggle"]
  , xf86AudioLower & volumectl ["decrease", "5"]
  , xf86AudioRaise & volumectl ["increase", "5"]
  ]
  where
    key & action = ((0, key), action)
    key # action = ((myModKey, key), action)
    key ! action = ((myModKey .|. shiftMask, key), action)

    infixr 0 #
    infixr 0 !

    toggleWorkspace = windows (W.view =<< W.tag . head . hiddenNonNSP)
    hiddenNonNSP = filter ((/= "NSP") . W.tag) . W.hidden

    keymap name = safeSpawn "keymap" [name]
    playerctl cmd = safeSpawn "playerctl" [cmd]
    volumectl cmd = safeSpawn "ponymix" ("-N" : cmd)

main :: IO ()
main = do
  safeSpawn "feh" ["--no-fehbg", "--bg-tile", "media/wallpaper"]
  safeSpawn "xsetroot" ["-cursor_name", "left_ptr"]
  safeSpawn "kupfer" ["--no-splash"]
  safeSpawnProg "nextcloud"
  hxmobar <- spawnPipe "xmobar .config/xmobar/xmobarrc"

  xmonad $ withUrgencyHook NoUrgencyHook def
    { borderWidth        = 1
    , workspaces         = myWorkspaces
    , layoutHook         = myLayoutHook
    , terminal           = "alacritty"
    , normalBorderColor  = colorBackground
    , focusedBorderColor = colorGreen
    , modMask            = myModKey
    , keys               = const myKeyMaps
    , logHook            = dynamicLogWithPP (myPP hxmobar)
    , manageHook         = myManageHook
    , handleEventHook    = docksEventHook
    , focusFollowsMouse  = False
    , clickJustFocuses   = False
    }
