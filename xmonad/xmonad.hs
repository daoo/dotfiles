import qualified Data.Map as M
import Data.Ratio
import System.IO
import XMonad
import XMonad.Actions.NoBorders
import XMonad.Actions.FloatKeys
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.Named
import qualified XMonad.Layout.NoBorders as B
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Prompt
import XMonad.Prompt.Shell
import qualified XMonad.StackSet as W
import XMonad.Util.Run
import XMonad.Util.Scratchpad 

-- Theme
winBorderFocused = "#303030"
winBorderNormal = "#202020"

panelFg = "#b8b8b8"
panelBg = "#202020"

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
  , moveTo "web"   [ "Firefox", "Navigator" ]
  , moveTo "code"  [ "Gvim" ]
  , moveTo "music" [ "spotify-win", "tuxguitar" ]

  , floatThose [ "MPlayer", "Wine", "xmessage" ]

  , [ isFullscreen --> (doF W.focusDown <+> doFullFloat) ]
  ]
  where
    moveTo w s   = [className =? a --> doShift w | a <- s]
    floatThose s = [className =? a --> doFloat | a <- s]

-- Layout Hook
myLayoutHook = onWorkspace "im" imLHook $
               onWorkspace "fullscreen" fullscreenLHook $
               defaultLHook
  where
    imLHook         = B.noBorders $ avoidStruts $ imLayout
    fullscreenLHook = B.noBorders $ Full 
    defaultLHook    = B.lessBorders ambiguity $ avoidStruts $ defaultLayout

    ambiguity  = (B.Combine B.Union B.Never B.OtherIndicated)

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
  , ppHiddenNoWindows = color viewsFg viewsBg
  , ppTitle           = color titleFg titleBg
  , ppWsSep           = " "
  , ppSep             = " | "
  , ppOutput          = hPutStrLn h
  }
  where
    color       = dzenColor
    noNSP ws    = asd (ws == "NSP") "" ws
    asd exp a b = if exp then a else b -- TODO: Find better name

-- Bars
barDefault, myLeftBar, myRightBar :: String
barDefault = "-fn '" ++ panelFont ++ "' -bg '" ++ panelBg ++ "' -fg '" ++ panelFg ++ "'"

myLeftBar  = "dzen2 -p -ta l -x 0 -y 0 -w 1200 " ++ barDefault
myRightBar = "conky -c ~/.xmonad/dzen_conkyrc | dzen2 -p -ta r -x 1200 -y 0 -w 720 " ++ barDefault

myModKey :: KeyMask
myModKey = mod4Mask


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
keysToAdd x = [ ((modMask x, xK_b), withFocused toggleBorder)
              , ((modMask x, xK_z), focusUrgent)
              , ((modMask x, xK_p), shellPrompt spConfig)
              , ((modMask x, xK_grave), scratchpadSpawnActionTerminal myTerm)
              , ((modMask x, xK_section), scratchpadSpawnActionTerminal myTerm)
              , ((modMask x, xK_f), spawn "xscreensaver-command --lock")
              , ((modMask x, xK_a), withFocused (keysMoveWindowTo (960, 540) (1 % 2, 1 % 2)))
              , ((modMask x, xK_z), spawn "firefox-nightly")
              , ((modMask x, xK_x), spawn "gvim")
              ]

-- Misc
myWorkspaces :: [WorkspaceId]
myWorkspaces = [ "im", "web", "code", "code2", "other", "music", "fullscreen", "void" ]

myTerm = "urxvt"

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

