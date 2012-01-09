module Main where

import Bar
import Common
import Config
import Keys
import Software

import Data.Map (union)

import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Util.Run
import XMonad.Util.Scratchpad

main :: IO ()
main = do
  s <- softwareDefault

  spawn conkyCmd
  d <- spawnPipe dzenCmd

  xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
    { modMask            = myModKey
    , borderWidth        = 1
    , normalBorderColor  = winBorderNormal
    , focusedBorderColor = winBorderFocused
    , workspaces         = myWorkspaces
    , focusFollowsMouse  = False

    , manageHook = myManageHook <+> manageDocks <+> scratchpadManageHookDefault
    , layoutHook = myLayoutHook

    , terminal = term s
    , keys     = \x -> union (newKeyMaps s) (keys defaultConfig x)
    , logHook  = myLogHook d }

  where
    conkyCmd = "conky -c ~/.xmonad/conkyrc 2> /dev/null | dzen2 -p " ++ barToString right
    dzenCmd  = "dzen2 -p " ++ barToString left

    (left, right) = (defaultBar, defaultBar)
