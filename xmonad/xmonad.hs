import Control.Concurrent (threadDelay)
import Data.List (intersperse)
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
import qualified XMonad.StackSet as W

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

-- Config
data Config = Config {
  host :: String,
  browser :: String,
  term :: String,
  editor :: String,
  lock :: String
} deriving (Show)

-- Theme
winBorderFocused = focusFg
winBorderNormal  = panelBg

panelFg    = "#b8b8b8"
panelBg    = "#2e3436"
titleFg    = "#d3d7cf"
titleBg    = panelBg
focusFg    = "#729fcf"
focusBg    = panelBg
urgentFg   = "#ef2929"
urgentBg   = panelBg
visibleFg  = "#ad7fa8"
visibleBg  = panelBg
occupiedFg = "#b8b8b8"
occupiedBg = panelBg
viewsFg    = "#757575"
viewsBg    = panelBg

panelFont = "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"

-- Manage Hook
myManageHook :: ManageHook
myManageHook = composeAll . concat $
  [ moves "im"    [ "Pidgin", "Skype" ]
  , moves "web"   [ "firefox-bin", "Firefox", "Navigator", "chromium" ]
  , moves "code"  [ "gvim" ]
  , moves "code2" [ "Eclipse" ]
  , moves "music" [ "tuxguitar" ]
  , moves "void"  [ "transmission-gtk" ]

  , floats name [ "MPlayer", "xmessage" ]
  , floats res [ "Dialog" ]
  , [ isFullscreen --> doFullFloat ]
  ]
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
myLogHook h           = dynamicLogWithPP $ defaultPP
  { ppUrgent          = color urgentFg urgentBg . dzenStrip
  , ppCurrent         = color focusFg focusBg
  , ppVisible         = color visibleFg visibleBg
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
type Bar = [String]

barToString :: Bar -> String
barToString = concat . intersperse " "

defaultBar, fullBar, leftBar, rightBar :: Bar
defaultBar  = ["-fn", show panelFont, "-bg", show panelBg, "-fg", show panelFg]
fullBar     = defaultBar ++ ["-x", "0",    "-y", "0", "-w", "1920", "-h", "13", "-ta", "c"]
leftBar     = defaultBar ++ ["-x", "0",    "-y", "0", "-w", "1200", "-h", "13", "-ta", "l"]
rightBar    = defaultBar ++ ["-x", "1200", "-y", "0", "-w", "720",  "-h", "13", "-ta", "r"]

-- Bar look
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
--keysToAdd :: Config -> KeyMask -> [((KeyMask, KeySym), X ())]
keysToAdd cfg modMask = fromList $
  [ ((modMask, xK_u), withFocused toggleBorder)
  , ((modMask, xK_s), toggleWS)
  , ((modMask, xK_g), goToSelected defaultGSConfig)

  -- Dynamic Workspaces
  , ((modMask, xK_b), removeWorkspace)
  , ((modMask, xK_n), selectWorkspace spConfig)
  , ((modMask, xK_m), withWorkspace spConfig (windows . W.shift))

  -- Terminals and stuff
  , ((modMask, xK_p), shellPrompt spConfig)
  , ((modMask, xK_grave), scratchpadSpawnActionTerminal $ term cfg)
  , ((modMask, xK_Return), spawn $ term cfg)

  -- Software
  , ((modMask, xK_z), spawn $ browser cfg)
  , ((modMask, xK_x), spawn $ editor cfg)
  , ((modMask, xK_c), spawn $ lock cfg)
  
  , ((0, 0x1008ff30), spawn "mpc prev")         -- XF86Favorites
  , ((0, 0x1008ff19), spawn "mpc next")         -- XF86Email
  , ((0, 0x1008ff12), spawn "pa-mute")          -- XF86AudioMute
  , ((0, 0x1008ff14), spawn "mpd-play-pause") ] -- XF86AudioPlay
  where
    toggleWS = windows $ W.view =<< W.tag . head . filter ((\ x -> x /= "NSP") . W.tag) . W.hidden

-- Misc
myModKey :: KeyMask
myModKey = mod4Mask

myWorkspaces :: [WorkspaceId]
myWorkspaces = [ "im", "web", "code", "code2", "other", "other2", "full", "void" ]

-- Main
main :: IO ()
main = do
  -- Spawn bars
  spawn $ "conky -c ~/.xmonad/dzen_conkyrc | dzen2 -p " ++ barToString rightBar
  d <- spawnPipe $ "dzen2 -p " ++ barToString leftBar

  -- Get some info
  h <- getHostName
  b <- getEnvDefault "BROWSER" "firefox"
  e <- getEnvDefault "GUI_EDITOR" "gvim"
  t <- getEnvDefault "TERMINAL" "urxvt"
  l <- getEnvDefault "SCREENSAVER" ""
  let cfg = Config { host = h, term = t , browser = b , editor = e, lock = l }

  -- Setup keys
  let a x = keysToAdd cfg (modMask x)
  let k x = union (a x) (keys defaultConfig x)

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
    , keys              = k }

getEnvDefault :: String -> String -> IO String
getEnvDefault env def = getEnv env `catch` (\_ -> return def)

getCommands :: IO [String]
getCommands = do
    home <- getEnvDefault "HOME" ""
    let d = home ++ "/.xmonad/bin"

    exists <- doesDirectoryExist d
    es <- if exists
      then getDirectoryContents d
      else return []
    return . uniqSort . filter ((/= '.') . head) $ es

shellPrompt :: XPConfig -> X ()
shellPrompt c = do
  cmds <- io $ getCommands
  mkXPrompt Shell c (getShellCompl cmds) (spawn . encodeOutput)

