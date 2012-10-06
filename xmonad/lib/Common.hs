module Common where

import Config

import Control.Applicative
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
myManageHook = composeAll
  [ "im"    `shift` ["Skype"]
  , "web"   `shift` ["Browser", "Firefox", "luakit"]
  , "code"  `shift` ["Gvim"]
  , "code2" `shift` ["Eclipse"]
  , "full"  `shift` ["Wine"]

  , comp (appName =?) ["MPlayer", "xmessage", "Options"]   --> doCenterFloat
  , comp (appName =?) ["Dialog", "Options", "Preferences"] --> doCenterFloat

  , (windowRole =? "Preferences") --> doCenterFloat
  ]
  where
    shift w a = comp (className =?) a --> doShift w
    comp f    = foldr ((<||>) . f) (return False)

    windowRole = stringProperty "WM_WINDOW_ROLE"

-- Layout Hook
myLayoutHook = onWorkspace "full" fullscreenLHook defaultLHook
  where
    defaultLHook    = avoidStruts $ lessBorders ambiguity $ tiled ||| Mirror tiled ||| Full
    fullscreenLHook = noBorders $ Full ||| tiled ||| Mirror tiled

    ambiguity = Combine Difference Screen OnlyFloat
    tiled     = Tall 1 0.03 0.5

-- Log Hook
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ myPP { ppOutput = hPutStrLn h }
