module Main where

import Bar
import Common
import Config
import Keys
import Software

import Data.Map (union)
import Network.BSD

import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Util.Run
import XMonad.Util.Scratchpad

zalbaarBar :: (Bar, Bar)
zalbaarBar = 
  ( defaultBar
    { barWidth = 1000
    , barX     = 0
    , barY     = 1080 - 13
    , barAlign = AlignLeft }
  , defaultBar
    { barWidth = 920
    , barX     = 1000
    , barY     = 1080 - 13
    , barAlign = AlignRight } )

chewbaccaBar :: (Bar, Bar)
chewbaccaBar =
  ( defaultBar
    { barWidth  = 683
    , barX      = 0
    , barY      = 768 - 13
    , barAlign  = AlignLeft }
  , defaultBar
    { barWidth = 683
    , barX     = 683
    , barY     = 768 - 13
    , barAlign = AlignRight } )

main :: IO ()
main = do
  h <- getHostName
  s <- softwareDefault

  let (left, right) = case h of
                        "zaalbar"   -> zalbaarBar
                        "freyyr"    -> zalbaarBar
                        "chewbacca" -> chewbaccaBar
                        _           -> undefined -- TODO
    
  spawn $ "conky -c ~/.xmonad/conky/" ++ h ++ " | dzen2 -p " ++ barToString right
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

