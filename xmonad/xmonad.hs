import Control.Monad
import Data.List
import qualified Data.Map as M
import Data.Ratio
import System.Cmd
import System.IO
import XMonad
import XMonad.Actions.NoBorders
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout
import XMonad.Layout.Combo
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Named
import qualified XMonad.Layout.NoBorders as B
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import qualified XMonad.StackSet as W
import XMonad.Util.Run
import XMonad.Util.Scratchpad 
import XMonad.Util.WindowProperties

-- Manage Hook
myManageHook = composeAll . concat $
  [ [ className =? a --> doShift "[im]"    | a <- myIMs ]
  , [ className =? b --> doShift "[music]" | b <- myMusics ]
  , [ myFloats       --> doFloat ]
  , [ isFullscreen   --> (doF W.focusDown <+> doFullFloat) ]
  ]
  where
    myFloats = ( className /=? "Navigator" <&&> className =? "Minefield" ) <||> className =? "MPlayer"
    myIMs    = [ "Pidgin", "Skype" ]
    myMusics = [ "spotify-win" ]

-- Layout Hook
myIMLayout = named "IM Grid" $ reflectHoriz $ withIM ratio rosters Grid
  where
    ratio   = 1 % 7
    rosters = pidgin
    pidgin  = (ClassName "Pidgin") `And` (Role "buddy_list")
    --skype   = (Title "daoo-- - Skype™ (Beta)")

myDefaultLayouts = tiled ||| Mirror tiled ||| Full
  where
    tiled = Tall 1 (3.0/100.0) (1.0/2.0)

myAmbiguity  = (B.Combine B.Union B.Never B.OtherIndicated)
myLayoutHook = onWorkspace "[im]" (B.noBorders $ avoidStruts $ myIMLayout) $
               onWorkspace "[fullscreen]" (B.noBorders $ Full) $
               (B.lessBorders myAmbiguity $ avoidStruts $ myDefaultLayouts)

-- Log Hook
myLogHook :: Handle -> X ()
myLogHook h   = dynamicLogWithPP $ defaultPP
  { ppUrgent  = c2 redColor . dzenStrip
  , ppCurrent = c1 greenColor
  , ppVisible = c1 blueColor
  , ppHidden  = c1 fgColor . noNSP
  , ppTitle   = c1 fgColor
  , ppWsSep   = " "
  , ppSep     = " | "
  , ppOutput  = hPutStrLn h
  }
  where
    c1 c     = dzenColor c ""
    c2 c     = dzenColor "" c
    noNSP ws = if ws == "NSP" then "" else ws

-- Simple
myDmenu = "exec `dmenu_path | dmenu -i -fn '" ++ myFont ++ "' -nb '" ++ bgColor ++ "' -nf '" ++ fgColor ++ "' -sb '" ++ greenColor ++ "' -sf '" ++ bgColor ++ "'`"

barDefault = "-fn '" ++ myFont ++ "' -bg '" ++ bgColor ++ "' -fg '" ++ fgColor ++ "'"

myLeftBar  = "dzen2 -p -ta l -x 0 -y 0 -w 1200 " ++ barDefault
myRightBar = "conky -c ~/.xmonad/dzen_conkyrc | dzen2 -p -ta r -x 1200 -y 0 -w 720 " ++ barDefault

myWorkspaces = [ "[im]", "[web]", "[code]", "[code2]", "[other]", "[music]", "[fullscreen]", "[8]", "[9]" ]

myTerm   = "urxvt"
myModKey = mod4Mask

myFont = "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"

fgColor    = "#f6f3e8"
bgColor    = "#242424"
redColor   = "#e5786d"
greenColor = "#95e454"
blueColor  = "#8ac6f2"

-- Keys
keysToAdd x = [ ((modMask x, xK_b), withFocused toggleBorder)
              , ((modMask x, xK_z), focusUrgent)
              , ((modMask x, xK_p), spawn myDmenu)
              , ((modMask x, xK_quoteleft), scratchpadSpawnActionTerminal myTerm)
              , ((modMask x, xK_section), scratchpadSpawnActionTerminal myTerm)
              , ((modMask x, xK_x), spawn "firefox-nightly")
              , ((modMask x, xK_y), spawn "setxkbmap usaswe")
              , ((modMask x, xK_u), spawn "setxkbmap us -variant colemak")
              , ((modMask x, xK_f), spawn "xscreensaver-command --lock")
              ]

keysToRemove :: XConfig Layout ->    [((KeyMask, KeySym),X ())]
keysToRemove x = []


main = do
  spawn myRightBar
  d <- spawnPipe myLeftBar

  xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
    { terminal           = myTerm
    , modMask            = myModKey
    , borderWidth        = 1
    , normalBorderColor  = fgColor
    , focusedBorderColor = blueColor 
    , workspaces         = myWorkspaces

    , manageHook = myManageHook <+> manageDocks <+> scratchpadManageHookDefault
    , layoutHook = myLayoutHook
    , logHook    = myLogHook d

    , focusFollowsMouse = False
    , keys              = k
    }
  where
    k x = M.union (a x) $ M.difference (keys defaultConfig x) (r x)
    a x = M.fromList $ keysToAdd x
    r x = M.fromList $ keysToRemove x

