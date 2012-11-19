module Main where

import Bar
import Common
import Config
import Keys

import Data.Map (union)

import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Util.Run
import XMonad.Util.Scratchpad

main :: IO ()
main = do
  spawn conkyCmd
  d <- spawnPipe dzenCmd

  xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
    { borderWidth        = 1
    , focusFollowsMouse  = False
    , focusedBorderColor = winBorderFocused
    , keys               = union newKeyMaps . keys defaultConfig
    , layoutHook         = myLayoutHook
    , logHook            = myLogHook d
    , manageHook         = myManageHook <+> manageDocks <+> scratchpadManageHookDefault
    , modMask            = myModKey
    , normalBorderColor  = winBorderNormal
    , terminal           = "urxvt"
    , workspaces         = myWorkspaces
    }

  where
    conkyCmd = "conky -c ~/.xmonad/conkyrc 2> /dev/null | dzen2 -p " ++ barToString right
    dzenCmd  = "dzen2 -p " ++ barToString left

    (left, right) = (defaultBar, defaultBar)
