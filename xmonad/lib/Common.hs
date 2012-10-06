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
  [ "im"    `shift` ["Pidgin", "Finch", "Skype"]
  , "web"   `shift` ["firefox-bin", "Firefox", "Navigator", "luakit"]
  , "code"  `shift` ["gvim"]
  , "code2" `shift` ["Eclipse"]
  , "other" `shift` ["LibreOffice"]
  , "full"  `shift` ["Wine"]

  , isApp ["MPlayer", "xmessage"] <||> isRes ["Dialog"] --> doCenterFloat
  ]
  where
    shift w a = isApp a --> doShift w

    comp f = foldr ((<||>) . f) (return False)

    isApp = comp ((<||>) <$> (appName =?) <*> (className =?))
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
