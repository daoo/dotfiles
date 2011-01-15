import qualified Data.Map as M
import Data.Ratio ((%))
import System.IO (Handle)
import XMonad
import XMonad.Actions.CycleWS (toggleWS)
import XMonad.Actions.NoBorders
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook hiding (Never)
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.Named (named)
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Reflect
import XMonad.Prompt
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Util.Run
import XMonad.Util.Scratchpad 

-- Theme
winBorderFocused = "#303030"
winBorderNormal  = "#202020"

panelFg    = "#b8b8b8"
panelBg    = "#202020"
titleFg    = "#fecf35"
titleBg    = "#202020"
focusFg    = "#fecf35"
focusBg    = "#202020"
urgentFg   = "#ff9800"
urgentBg   = "#202020"
occupiedFg = "#b8b8b8"
occupiedBg = "#202020"
viewsFg    = "#757575"
viewsBg    = "#202020"

panelFont = "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"

-- Manage Hook
myManageHook :: ManageHook
myManageHook = composeAll . concat $
  [ moveTo "im"    [ "Pidgin", "Skype" ]
  , moveTo "web"   [ "Navigator" ]
  , moveTo "code"  [ "gvim" ]
  , moveTo "code2" [ "Eclipse" ]
  , moveTo "music" [ "spotify-win", "tuxguitar" ]
  , moveTo "void"  [ "explorer.exe", "transmission-gtk" ]

  , floatThose [ "MPlayer", "Wine", "xmessage" ]
  ]
  where
    moveTo w   = map (\a -> match a --> doShift w)
    floatThose = map (\a -> match a --> doFloat)
    match a    = appName =? a <||> className =? a

-- Layout Hook
myLayoutHook = onWorkspace "im" imLHook $
               onWorkspace "fullscreen" fullscreenLHook $
               defaultLHook
  where
    imLHook         = noBorders $ avoidStruts $ imLayout
    fullscreenLHook = noBorders $ Full 
    defaultLHook    = lessBorders ambiguity $ avoidStruts $ defaultLayout

    ambiguity  = (Combine Union Never OtherIndicated)

    defaultLayout = tiled ||| Mirror tiled ||| Full
    tiled         = Tall 1 (3.0/100.0) (1.0/2.0)

    imLayout = named "IM Grid" $ reflectHoriz $ withIM (1%7) rosters Grid
    rosters  = (ClassName "Pidgin") `And` (Role "buddy_list")

-- Log Hook
myLogHook :: Handle -> X ()
myLogHook h           = dynamicLogWithPP $ defaultPP
  { ppUrgent          = color urgentFg urgentBg . dzenStrip
  , ppCurrent         = color focusFg focusBg
  , ppVisible         = color occupiedFg occupiedBg
  , ppHidden          = color occupiedFg occupiedBg . noNSP
  , ppHiddenNoWindows = color viewsFg viewsBg . noNSP
  , ppTitle           = color titleFg titleBg
  , ppWsSep           = " "
  , ppSep             = " | "
  , ppOutput          = hPutStrLn h
  }
  where
    color     = dzenColor
    noNSP ws  = c (ws == "NSP") "" ws
    c exp a b = if exp then a else b -- TODO: Find better name

-- Bars
barDefault, myLeftBar, myRightBar :: String
barDefault = "-fn '" ++ panelFont ++ "' -bg '" ++ panelBg ++ "' -fg '" ++ panelFg ++ "'"

myLeftBar  = "dzen2 -p -ta l -x 0 -y 0 -w 1200 " ++ barDefault
myRightBar = "conky -c ~/.xmonad/dzen_conkyrc | dzen2 -p -ta r -x 1200 -y 0 -w 720 " ++ barDefault

spConfig :: XPConfig
spConfig = defaultXPConfig
  { font              = panelFont
  , bgColor           = panelBg
  , fgColor           = panelFg
  , bgHLight          = focusFg
  , position          = Top
  , promptBorderWidth = 0
  }

-- Keys
keysToAdd :: XConfig l -> [((KeyMask, KeySym), X ())]
keysToAdd x = map (\ (k, a) -> ((modMask x, k), a) ) $
  [ (xK_b, withFocused toggleBorder)
  , (xK_p, shellPrompt spConfig)
  , (xK_grave, scratchpadSpawnActionTerminal myTerm)
  , (xK_f, spawn "xscreensaver-command --lock")
  , (xK_z, spawn myBrowser)
  , (xK_x, spawn myEditor)
  , (xK_w, toggleWS)
  ]

-- Misc
myModKey :: KeyMask
myModKey = mod4Mask

myWorkspaces :: [WorkspaceId]
myWorkspaces = [ "im", "web", "code", "code2", "other", "music", "full", "void" ]

myTerm    = "urxvt"
myBrowser = "firefox-nightly"
myEditor  = "gvim"

-- Main

main :: IO ()
main = do
  spawn myRightBar
  d <- spawnPipe myLeftBar

  xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
    { terminal           = myTerm
    , modMask            = myModKey
    , borderWidth        = 1
    , normalBorderColor  = winBorderNormal
    , focusedBorderColor = winBorderFocused 
    , workspaces         = myWorkspaces

    , manageHook = myManageHook <+> manageDocks <+> scratchpadManageHookDefault
    , layoutHook = myLayoutHook
    , logHook    = myLogHook d

    , focusFollowsMouse = False
    , keys              = k
    }
  where
    k x = M.union (a x) (keys defaultConfig x)
    a x = M.fromList $ keysToAdd x

