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
import XMonad.Prompt
import XMonad.Prompt.Shell
import qualified XMonad.StackSet as W
import XMonad.Util.Run
import XMonad.Util.Scratchpad 
import XMonad.Util.WindowProperties

-- Manage Hook
myManageHook = composeAll . concat $
  [ [ className =? a --> doShift "[im]"    | a <- imShifts ]
  , [ className =? a --> doShift "[music]" | a <- musicShifts ]
  , [ className =? a --> doFloat           | a <- floats ]
  , [ isFullscreen   --> (doF W.focusDown <+> doFullFloat) ]
  ]
  where
    floats      = [ "MPlayer" ]
    imShifts    = [ "Pidgin", "Skype" ]
    musicShifts = [ "spotify-win" ]

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
  { ppUrgent  = c2 barRedColor . dzenStrip
  , ppCurrent = c1 barGreenColor
  , ppVisible = c1 barBlueColor
  , ppHidden  = c1 barFgColor . noNSP
  , ppTitle   = c1 barFgColor
  , ppWsSep   = " "
  , ppSep     = " | "
  , ppOutput  = hPutStrLn h
  }
  where
    c1 c     = dzenColor c ""
    c2 c     = dzenColor "" c
    noNSP ws = if ws == "NSP" then "" else ws

-- Simple
barDefault = "-fn '" ++ barFont ++ "' -bg '" ++ barBgColor ++ "' -fg '" ++ barFgColor ++ "'"

myLeftBar  = "dzen2 -p -ta l -x 0 -y 0 -w 1200 " ++ barDefault
myRightBar = "conky -c ~/.xmonad/dzen_conkyrc | dzen2 -p -ta r -x 1200 -y 0 -w 720 " ++ barDefault

myWorkspaces = [ "[im]", "[web]", "[code]", "[code2]", "[other]", "[music]", "[fullscreen]", "[8]", "[9]" ]

myTerm   = "urxvt"
myModKey = mod4Mask

barFont = "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"

barFgColor    = "#f6f3e8"
barBgColor    = "#242424"
barRedColor   = "#e5786d"
barGreenColor = "#95e454"
barBlueColor  = "#8ac6f2"

-- Keys
keysToAdd x = [ ((modMask x, xK_b), withFocused toggleBorder)
              , ((modMask x, xK_z), focusUrgent)
              , ((modMask x, xK_p), shellPrompt defaultXPConfig)
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
    , normalBorderColor  = barFgColor
    , focusedBorderColor = barBlueColor 
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

