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

  (left, right) = (defaultBar, defaultBar)

  spawn $ "conky -c ~/.xmonad/conkyrc | dzen2 -p " ++ barToString right
  d <- spawnPipe $ "dzen2 -p " ++ barToString left

  -- Setup keys
  let a x = keysToAdd s (modMask x)
  let k x = union (a x) (keys defaultConfig x)

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
    , keys     = k
    , logHook  = myLogHook d }

