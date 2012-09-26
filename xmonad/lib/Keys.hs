module Keys (newKeyMaps, myModKey) where

import Config
import Prompt

import Data.Map (Map(), fromList)

import XMonad
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.GridSelect
import XMonad.Actions.NoBorders
import XMonad.StackSet (hidden, shift, tag, view, greedyView)
import XMonad.Util.Scratchpad

-- Modkey
myModKey :: KeyMask
myModKey = mod4Mask

-- Some media keys
xf86AudioLower, xf86AudioMute, xf86AudioPlay, xf86AudioRaise, xf86Email,
  xf86Favorites, xf86TouchpadToggle :: KeySym
xf86AudioLower     = 0x1008ff11
xf86AudioMute      = 0x1008ff12
xf86AudioPlay      = 0x1008ff14
xf86AudioRaise     = 0x1008ff13
xf86Email          = 0x1008ff19
xf86Favorites      = 0x1008ff30
xf86TouchpadToggle = 0x1008ffa9

-- For programmers dvorak
dvorakMaps :: KeyMask -> (WorkspaceId -> X ()) -> [((KeyMask, KeySym), X ())]
dvorakMaps m f = zip (zip (repeat m) workspaceKeys) (map f myWorkspaces)
  where
    workspaceKeys = [ xK_ampersand, xK_bracketleft, xK_braceleft, xK_braceright
                    , xK_parenleft, xK_equal, xK_asterisk, xK_parenright, xK_plus
                    , xK_bracketright, xK_exclam ]

newKeyMaps :: Map (KeyMask, KeySym) (X ())
newKeyMaps = fromList $
  [ ((myModKey, xK_u), withFocused toggleBorder)
  , ((myModKey, xK_o), toggleWS)
  , ((myModKey, xK_r), goToSelected defaultGSConfig)

  -- Dynamic Workspaces
  , ((myModKey .|. shiftMask, xK_g), removeEmptyWorkspace)
  , ((myModKey, xK_g), selectWorkspace myXPConfig)
  , ((myModKey, xK_c), withWorkspace myXPConfig (windows . shift))

  -- Terminals and stuff
  , ((myModKey, xK_p), launchPrompt myXPConfig)
  , ((myModKey, xK_i), scratchpadSpawnActionTerminal "urxvt")
  , ((myModKey, xK_Return), spawn "urxvt")

  -- Software
  , ((myModKey, xK_x), spawn "firefox")
  , ((myModKey, xK_b), spawn "gvim")

  -- Multimedia keys
  , ((0, xf86AudioMute), spawn "alsa-mute")
  , ((0, xf86AudioLower), spawn "amixer -q set Master 1-")
  , ((0, xf86AudioRaise), spawn "amixer -q set Master 1+")
  ]

  -- For Programmers Dvorak
  ++ dvorakMaps myModKey (windows . greedyView)
  ++ dvorakMaps (myModKey .|. shiftMask) (windows . shift)
  where
    toggleWS = windows $ view =<< tag . head . filter ((/= "NSP") . tag) . hidden
