module Common where

import Config

import System.IO (Handle)

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Util.Run

-- Manage Hook
myManageHook :: ManageHook
myManageHook = composeAll . concat $
  [ moves "im"    [ "Pidgin", "Finch" ]
  , moves "web"   [ "firefox-bin", "Firefox", "Navigator", "luakit" ]
  , moves "code"  [ "gvim" ]
  , moves "code2" [ "Eclipse" ]
  , moves "full"  [ "Wine" ]
  , moves "void"  [ "Skype" ]

  , floats name [ "MPlayer", "xmessage" ]
  , floats res [ "Dialog" ]
  , [ isFullscreen --> doFullFloat ] ]
  where
    moves w  = map (\a -> name a --> doShift w)
    floats m = map (\a -> m a --> doCenterFloat)

    name a = appName  =? a <||> className =? a
    res a  = resource =? a

-- Layout Hook
myLayoutHook = onWorkspace "full" fullscreenLHook $
               defaultLHook
  where
    defaultLHook    = avoidStruts $ lessBorders ambiguity $ defaultLayout
    fullscreenLHook = noBorders $ fullFirstLayout 

    ambiguity = (Combine Difference Screen OnlyFloat)

    fullFirstLayout = Full ||| tiled ||| Mirror tiled
    defaultLayout   = tiled ||| Mirror tiled ||| Full
    tiled           = Tall 1 (3.0/100.0) (1.0/2.0)

-- Log Hook
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ myPP { ppOutput = hPutStrLn h }

