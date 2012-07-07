module Keys (newKeyMaps, myModKey) where

import Config
import Prompt

import Data.Map hiding (filter, map)

import XMonad
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.GridSelect
import XMonad.Actions.NoBorders
import XMonad.StackSet (hidden, shift, tag, view)
import XMonad.Util.Scratchpad

-- Modkey
myModKey :: KeyMask
myModKey = mod4Mask

-- For programmers dvorak
{-workspaceKeys :: [KeySym]
workspaceKeys = [ xK_ampersand, xK_bracketleft, xK_braceleft, xK_braceright
                , xK_parenleft, xK_equal, xK_asterisk, xK_parenright, xK_plus
                , xK_bracketright, xK_exclam ]-}

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

newKeyMaps :: Map (KeyMask, KeySym) (X ())
newKeyMaps = fromList
  [ ((myModKey, xK_u), withFocused toggleBorder)
  , ((myModKey, xK_o), toggleWS)
  , ((myModKey, xK_d), goToSelected defaultGSConfig)

  -- Dynamic Workspaces
  , ((myModKey, xK_y), removeEmptyWorkspace)
  , ((myModKey, xK_f), selectWorkspace myXPConfig)
  , ((myModKey, xK_g), withWorkspace myXPConfig (windows . shift))

  -- Terminals and stuff
  , ((myModKey, xK_p), launchPrompt myXPConfig)
  , ((myModKey, xK_i), scratchpadSpawnActionTerminal "urxvt")
  , ((myModKey, xK_Return), spawn "urxvt")

  -- Software
  , ((myModKey, xK_x), spawn "firefox")
  , ((myModKey, xK_b), spawn "gvim")
  , ((myModKey, xK_m), spawn "slimlock")

  -- Multimedia keys
  , ((0, xf86AudioMute), spawn "alsa-mute")
  , ((0, xf86AudioLower), spawn "amixer -q set Master 1-")
  , ((0, xf86AudioRaise), spawn "amixer -q set Master 1+")
  ]

  -- For Programmers Dvorak
  {-++ mapWS myModKey (windows . greedyView) wsWithKeys
  ++ mapWS (myModKey .|. shiftMask) (\ i -> (windows $ shift i) >> (windows $ greedyView i)) wsWithKeys-}
  where
    --wsWithKeys   = zip myWorkspaces workspaceKeys
    --mapWS m a ws = map (\ (i, k) -> ((m, k), a i)) ws

    -- Ignore NSP, aquire workspaces
    toggleWS = windows $ view =<< tag . head . filter ((/= "NSP") . tag) . hidden
