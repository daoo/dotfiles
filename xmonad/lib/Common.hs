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
  [ move "im"    [ "Pidgin", "Finch" ]
  , move "web"   [ "firefox-bin", "Firefox", "Navigator", "luakit" ]
  , move "code"  [ "gvim" ]
  , move "code2" [ "Eclipse" ]
  , move "other" [ "LibreOffice" ]
  , move "full"  [ "Wine" ]
  , move "void"  [ "Skype" ]

  , float name [ "MPlayer", "xmessage" ]
  , float res [ "Dialog" ] ]
  where
    move to = map (\a -> name a --> doShift to)
    float f = map (\a -> f a --> doCenterFloat)

    name a = appName =? a <||> className =? a
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

