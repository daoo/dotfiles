module Common where

import Config

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
myManageHook = composeAll hooks
  where
    hooks = 
      [ isApp ["Pidgin", "Finch"]                               --> doShift "im"
      , isApp ["firefox-bin", "Firefox", "Navigator", "luakit"] --> doShift "web"
      , isApp ["gvim"]                                          --> doShift "code"
      , isApp ["Eclipse"]                                       --> doShift "code2"
      , isApp ["LibreOffice"]                                   --> doShift "other"
      , isApp ["Wine"]                                          --> doShift "full"
      , isApp ["Skype"]                                         --> doShift "void"
      , isApp ["MPlayer", "xmessage"] <||> isRes ["Dialog"]     --> doCenterFloat
      ]

    isApp :: [String] -> Query Bool
    isApp = foldr1 (<||>) . map name

    isRes :: [String] -> Query Bool
    isRes = foldr1 (<||>) . map (resource =?)

    name a = appName =? a <||> className =? a

-- Layout Hook
myLayoutHook = onWorkspace "full" fullscreenLHook defaultLHook
  where
    defaultLHook    = avoidStruts $ lessBorders ambiguity defaultLayout
    fullscreenLHook = noBorders fullFirstLayout 

    ambiguity = Combine Difference Screen OnlyFloat

    fullFirstLayout = Full ||| tiled ||| Mirror tiled
    defaultLayout   = tiled ||| Mirror tiled ||| Full
    tiled           = Tall 1 0.03 0.5

-- Log Hook
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ myPP { ppOutput = hPutStrLn h }

