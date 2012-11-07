module Common where

import Config

import Control.Applicative
import Data.Ratio
import System.IO (Handle)

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ICCCMFocus
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Reflect
import XMonad.Util.Run

-- Manage Hook
myManageHook :: ManageHook
myManageHook = composeAll
  [ "im"    `shift` ["Skype"]
  , "web"   `shift` ["Browser", "Firefox", "luakit"]
  , "code"  `shift` ["Gvim"]
  , "code2" `shift` ["Eclipse"]
  , "full"  `shift` ["Wine"]

  , comp (appName =?) ["MPlayer", "xmessage"] --> doCenterFloat
  , comp (className =?) ["Dialog"] --> doCenterFloat

  , (wmName =? "Options")           --> doCenterFloat
  , (wmWindowRole =? "Preferences") --> doCenterFloat
  ]
  where
    shift w a = comp (className =?) a --> doShift w
    comp f    = foldr ((<||>) . f) (return False)

    wmWindowRole = stringProperty "WM_WINDOW_ROLE"
    wmName       = stringProperty "WM_NAME"

-- Layout Hook
myLayoutHook = onWorkspace "im" imLayout $ onWorkspace "full" fullLayout defaultLayout
  where
    defaultLayout = avoidStruts $ lessBorders ambiguity $ tiled ||| Mirror tiled ||| Full
    imLayout      = avoidStruts $ lessBorders ambiguity $ reflectHoriz im
    fullLayout    = noBorders $ Full ||| tiled ||| Mirror tiled

    ambiguity = Combine Difference Screen OnlyFloat
    tiled     = Tall 1 0.03 0.5
    im        = withIM (1%7) skypeBuddyList (Grid ||| Full)

    skypeBuddyList = Title "daoo-- - Skype\8482"

-- Log Hook
myLogHook :: Handle -> X ()
myLogHook h = takeTopFocus >> dynamicLogWithPP (myPP { ppOutput = hPutStrLn h })
