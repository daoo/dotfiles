module Common where

import Config

import Control.Applicative
import Data.Ratio
import System.IO (Handle)

import XMonad
import XMonad.Hooks.DynamicLog
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
  [ className    =? "Skype"                --> doShift "im"
  , className    =? "Browser"              --> doShift "web"
  , className    =? "Firefox"              --> doShift "web"
  , className    =? "Gvim"                 --> doShift "code"
  , className    =? "Eclipse"              --> doShift "code2"
  , className    =? "Wine"                 --> doShift "full"
  , appName      =? "MPlayer"              --> doCenterFloat
  , className    =? "Dialog"               --> doCenterFloat
  , wmName       =? "Options"              --> doCenterFloat
  , wmWindowRole =? "Preferences"          --> doCenterFloat
  , wmWindowRole =? "GtkFileChooserDialog" --> doCenterFloat
  , className    =? "Steam"                --> doIgnore
  ]
  where
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
myLogHook h = dynamicLogWithPP (myPP { ppOutput = hPutStrLn h })
