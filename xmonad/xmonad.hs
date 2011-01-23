import qualified Data.Map as M
import Data.Ratio ((%))
import System.IO (Handle)
import System.Environment
import XMonad
import XMonad.Actions.NoBorders
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook hiding (Never)
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.Named (named)
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Reflect
import XMonad.Layout.Simplest
import XMonad.Prompt
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Util.Run
import XMonad.Util.Scratchpad 
import qualified XMonad.StackSet as W

data Config = Config {
  browser :: String,
  term :: String,
  editor :: String
} deriving (Show)

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
  [ moves "im"    [ "Pidgin", "Skype" ]
  , moves "web"   [ "firefox-bin", "Firefox", "Navigator" ]
  , moves "code"  [ "gvim" ]
  , moves "code2" [ "Eclipse" ]
  , moves "music" [ "spotify-win", "spotify.exe", "tuxguitar" ]
  , moves "void"  [ "explorer.exe", "transmission-gtk" ]

  , floats wmClass [ "MPlayer", "Wine", "xmessage" ]
  , floats res [ "Dialog" ]
  , [ isFullscreen --> doFullFloat ]
  ]
  where
    moves w  = map (\a -> wmClass a --> doShift w)
    floats m = map (\a -> m a --> doFloat)

    wmClass a = appName =? a <||> className =? a
    res a     = resource =? a

-- Layout Hook
myLayoutHook = onWorkspace "im" imLHook $
               onWorkspace "full" fullscreenLHook $
               defaultLHook
  where
    imLHook         = noBorders $ avoidStruts $ imLayout
    fullscreenLHook = noBorders $ fullFirstLayout 
    defaultLHook    = lessBorders ambiguity $ avoidStruts defaultLayout

    ambiguity  = (Combine Union Never OtherIndicated)

    fullFirstLayout = Full ||| tiled ||| Mirror tiled
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
    noNSP ws  = if (ws == "NSP") then "" else ws

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
keysToAdd :: Config -> KeyMask -> [((KeyMask, KeySym), X ())]
keysToAdd cfg modMask = map (\ (k, a) -> ((modMask, k), a) ) $
  [ (xK_b, withFocused toggleBorder)
  , (xK_w, toggleWS)

  , (xK_p, shellPrompt spConfig)
  , (xK_grave, scratchpadSpawnActionTerminal $ term cfg)
  , (xK_f, spawn "xscreensaver-command --lock")
  , (xK_z, spawn $ browser cfg)
  , (xK_x, spawn $ editor cfg)
  , (xK_Return, spawn $ term cfg)
  ]
  where
    toggleWS = windows $ W.view =<< W.tag . head . filter ((\ x -> x /= "NSP") . W.tag) . W.hidden

-- Misc
myModKey :: KeyMask
myModKey = mod4Mask

myWorkspaces :: [WorkspaceId]
myWorkspaces = [ "im", "web", "code", "code2", "other", "music", "full", "void" ]

-- Main

main :: IO ()
main = do
  spawn myRightBar
  d <- spawnPipe myLeftBar

  b <- getEnvDefault "BROWSER" "firefox"
  e <- getEnvDefault "GUI_EDITOR" "gvim"
  t <- getEnvDefault "TERMINAL" "urxvt"

  let cfg = Config { term = t , browser = b , editor = e }
  let a x = M.fromList $ keysToAdd cfg (modMask x)
  let k x = M.union (a x) (keys defaultConfig x)

  xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
    { terminal           = t
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

getEnvDefault :: String -> String -> IO String
getEnvDefault env def = getEnv env `catch` (\_ -> return def)

