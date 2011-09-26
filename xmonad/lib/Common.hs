module Common where

import Theme

import Data.Ratio ((%))
import System.IO (Handle)

-- Xmonad
import XMonad
import XMonad.Util.Run

-- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks

-- Layouts
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.Named (named)
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Reflect

-- Manage Hook
myManageHook :: ManageHook
myManageHook = composeAll . concat $
  [ moves "im"    [ "Pidgin", "Skype" ]
  , moves "web"   [ "firefox-bin", "Firefox", "Navigator" ]
  , moves "code"  [ "gvim" ]
  , moves "code2" [ "Eclipse" ]

  , floats name [ "MPlayer", "xmessage" ]
  , floats res [ "Dialog" ]
  , [ isFullscreen --> doFullFloat ] ]
  where
    moves w  = map (\a -> name a --> doShift w)
    floats m = map (\a -> m a --> doCenterFloat)

    name a = appName  =? a <||> className =? a
    res a  = resource =? a

-- Layout Hook
myLayoutHook = onWorkspace "im" imLHook $
               onWorkspace "full" fullscreenLHook $
               defaultLHook
  where
    defaultLHook    = avoidStruts $ lessBorders ambiguity $ defaultLayout
    imLHook         = avoidStruts $ lessBorders ambiguity $ imLayout
    fullscreenLHook = noBorders $ fullFirstLayout 

    ambiguity = (Combine Difference Screen OnlyFloat)

    fullFirstLayout = Full ||| tiled ||| Mirror tiled
    defaultLayout   = tiled ||| Mirror tiled ||| Full
    tiled           = Tall 1 (3.0/100.0) (1.0/2.0)

    imLayout = named "IM Grid" $ reflectHoriz $ withIM (1%7) rosters Grid
    rosters  = (ClassName "Pidgin") `And` (Role "buddy_list")

-- Log Hook
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ myPP { ppOutput = hPutStrLn h }

