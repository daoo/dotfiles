module Common where

import Theme

import Control.Concurrent (threadDelay)
import Data.List (intersperse, find)
import Data.Map (union, fromList)
import Data.Ratio ((%))
import Network.BSD
import Prelude hiding (Left, Right)
import System.Directory
import System.Environment
import System.IO (Handle)

-- Xmonad
import XMonad
import XMonad.Prompt
import XMonad.Prompt.Shell hiding (shellPrompt, getCommands)
import XMonad.Util.Dzen as D
import XMonad.Util.Run
import XMonad.Util.Scratchpad 
import qualified XMonad.StackSet as S

-- Actions
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.NoBorders
import XMonad.Actions.GridSelect

-- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook hiding (Never)

-- Layouts
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.Named (named)
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Reflect
import XMonad.Layout.Simplest

-- Manage Hook
myManageHook :: ManageHook
myManageHook = composeAll . concat $
  [ moves "im"    [ "Pidgin", "Skype" ]
  , moves "web"   [ "firefox-bin", "Firefox", "Navigator" ]
  , moves "code"  [ "gvim" ]
  , moves "code2" [ "Eclipse" ]
  , moves "music" [ "tuxguitar" ]
  , moves "void"  [ "transmission-gtk" ]

  , floats name [ "MPlayer", "xmessage" ]
  , floats res [ "Dialog" ]
  , [ isFullscreen --> doFullFloat ] ]
  where
    moves w  = map (\a -> name a --> doShift w)
    floats m = map (\a -> m a --> doFloat)

    name a = appName  =? a <||> className =? a
    res a  = resource =? a

-- Layout Hook
myLayoutHook = onWorkspace "im" imLHook $
               onWorkspace "full" fullscreenLHook $
               defaultLHook
  where
    defaultLHook    = avoidStruts $ lessBorders ambiguity $ defaultLayout
    imLHook         = avoidStruts $ lessBorders ambiguity $ imLayout
    fullscreenLHook = noBorders $ fullFirstLayout 

    ambiguity = (Combine Difference Screen OnlyFloat)

    fullFirstLayout = Full ||| tiled ||| Mirror tiled
    defaultLayout   = tiled ||| Mirror tiled ||| Full
    tiled           = Tall 1 (3.0/100.0) (1.0/2.0)

    imLayout = named "IM Grid" $ reflectHoriz $ withIM (1%7) rosters Grid
    rosters  = (ClassName "Pidgin") `And` (Role "buddy_list")

-- Log Hook
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ defaultPP
  { ppUrgent          = color urgentFg urgentBg . dzenStrip
  , ppCurrent         = color focusFg focusBg
  , ppVisible         = color visibleFg visibleBg
  , ppHidden          = color occupiedFg occupiedBg . noNSP
  , ppHiddenNoWindows = color viewsFg viewsBg . noNSP
  , ppTitle           = color titleFg titleBg
  , ppWsSep           = " "
  , ppSep             = " | "
  , ppOutput          = hPutStrLn h }
  where
    color     = dzenColor
    noNSP ws  = if (ws == "NSP") then "" else ws
