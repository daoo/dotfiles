import Config
import Keys
import Theme
import Common
import Prompt

import Data.Map (union)
import Network.BSD
import System.IO

-- Xmonad
import XMonad
import XMonad.Util.Run
import XMonad.Util.Scratchpad 

-- Hooks
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook

zalbaarBar = 
  ( defaultBar
    { barWidth = 1000
    , barX     = 0
    , barAlign = AlignLeft }
  , defaultBar
    { barWidth = 920
    , barX     = 1000
    , barAlign = AlignRight } )

chewbaccaBar =
  ( defaultBar
    { barWidth = 683
    , barX     = 0
    , barAlign = AlignLeft }
  , defaultBar
    { barWidth = 683
    , barX     = 683
    , barAlign = AlignRight } )


main :: IO ()
main = do
  h <- getHostName
  s <- softwareDefault

  let (left, right) = case h of
                        "zaalbar"   -> zalbaarBar
                        "chewbacca" -> chewbaccaBar
    
  spawn $ "conky -c ~/.xmonad/conky/" ++ h ++ " | dzen2 -p " ++ barToString left
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
