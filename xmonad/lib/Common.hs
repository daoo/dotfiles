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
  [ isApp ["Pidgin", "Finch"]                               --> doShift "im"
  , isApp ["firefox-bin", "Firefox", "Navigator", "luakit"] --> doShift "web"
  , isApp ["gvim"]                                          --> doShift "code"
  , isApp ["Eclipse"]                                       --> doShift "code2"
  , isApp ["LibreOffice"]                                   --> doShift "other"
  , isApp ["Wine"]                                          --> doShift "full"
  , isApp ["Skype"]                                         --> doShift "void"
  , isApp ["MPlayer", "xmessage"] <||> isRes ["Dialog"]     --> doCenterFloat
  ]
  where
    comp f = foldr ((<||>) . f) (return False)

    isApp = comp ((<||>) . (appName =?) <*> (className =?))
    isRes = comp (resource =?)

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
