module Keys
  ( newKeyMaps
  , myModKey
  ) where

import Config
import Prompt
import XMonad
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.GridSelect
import XMonad.Actions.NoBorders
import XMonad.StackSet (hidden, shift, tag, view, greedyView)
import XMonad.Util.Run
import XMonad.Util.Scratchpad
import qualified Data.Map as M

myModKey :: KeyMask
myModKey = mod4Mask

xf86AudioLower, xf86AudioMute, xf86AudioPlay, xf86AudioRaise, xf86Email,
  xf86Favorites, xf86TouchpadToggle :: KeySym
xf86AudioLower     = 0x1008ff11
xf86AudioMute      = 0x1008ff12
xf86AudioPlay      = 0x1008ff14
xf86AudioRaise     = 0x1008ff13
xf86Email          = 0x1008ff19
xf86Favorites      = 0x1008ff30
xf86TouchpadToggle = 0x1008ffa9

{-# INLINE dvorakMaps #-}
dvorakMaps :: KeyMask -> (WorkspaceId -> X ()) -> [((KeyMask, KeySym), X ())]
dvorakMaps m f = zipWith (\k w -> ((m, k), f w)) workspaceKeys myWorkspaces
  where
    workspaceKeys = [ xK_ampersand, xK_bracketleft, xK_braceleft, xK_braceright
                    , xK_parenleft, xK_equal, xK_asterisk, xK_parenright, xK_plus
                    , xK_bracketright, xK_exclam ]

newKeyMaps :: M.Map (KeyMask, KeySym) (X ())
newKeyMaps = M.fromList $
  [ ((myModKey, xK_u), withFocused toggleBorder)
  , ((myModKey, xK_o), toggleWS)
  , ((myModKey, xK_r), goToSelected defaultGSConfig)

  , ((myModKey .|. shiftMask, xK_g), removeEmptyWorkspace)
  , ((myModKey, xK_g), selectWorkspace myXPConfig)
  , ((myModKey, xK_c), withWorkspace myXPConfig (windows . shift))

  , ((myModKey, xK_p), launchPrompt myXPConfig)
  , ((myModKey, xK_i), scratchpadSpawnActionTerminal "/usr/bin/urxvt")
  , ((myModKey, xK_Return), safeSpawnProg "/usr/bin/urxvt")

  , ((myModKey, xK_x), safeSpawnProg "/usr/bin/firefox")
  , ((myModKey, xK_b), safeSpawnProg "/usr/bin/gvim")
  ]

  ++ dvorakMaps myModKey (windows . greedyView)
  ++ dvorakMaps (myModKey .|. shiftMask) (windows . shift)
  where
    toggleWS = windows $ view =<< tag . head . filter ((/= "NSP") . tag) . hidden
