module Keys (keysToAdd) where

import Config
import Prompt
import Software

import Data.Map hiding (filter, map)

import qualified XMonad.StackSet as S

import XMonad

-- Actions
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.GridSelect
import XMonad.Actions.NoBorders

import XMonad.Util.Scratchpad 

workspaceKeys :: [KeySym]
workspaceKeys = [ xK_ampersand, xK_bracketleft, xK_braceleft, xK_braceright
                , xK_parenleft, xK_equal, xK_asterisk, xK_parenright, xK_plus
                , xK_bracketright, xK_exclam ]

keysToAdd :: Software -> KeyMask -> Map (KeyMask, KeySym) (X ())
keysToAdd cfg modifier = fromList $
  [ ((modifier, xK_u), withFocused toggleBorder)
  , ((modifier, xK_o), toggleWS)
  , ((modifier, xK_d), goToSelected defaultGSConfig)

  -- Dynamic Workspaces
  , ((modifier, xK_y), removeWorkspace)
  , ((modifier, xK_f), selectWorkspace myXPConfig)
  , ((modifier, xK_g), withWorkspace myXPConfig (windows . S.shift))

  -- Terminals and stuff
  , ((modifier, xK_p), shellPrompt myXPConfig)
  , ((modifier, xK_dollar), scratchpadSpawnActionTerminal $ term cfg)
  , ((modifier, xK_asciitilde), scratchpadSpawnActionTerminal $ term cfg)
  , ((modifier, xK_grave), scratchpadSpawnActionTerminal $ term cfg)
  , ((modifier, xK_Return), spawn $ term cfg)

  -- Software
  , ((modifier, xK_x), spawn $ browser cfg)
  , ((modifier, xK_b), spawn $ editor cfg)
  , ((modifier, xK_m), spawn $ lock cfg)
  
  -- Multimedia keys
  , ((0, 0x1008ff30), spawn "mpc prev > /dev/null") -- XF86Favorites
  , ((0, 0x1008ff19), spawn "mpc next > /dev/null") -- XF86Email
  , ((0, 0x1008ff12), spawn "pa-mute")              -- XF86AudioMute
  , ((0, 0x1008ff14), spawn "mpd-play-pause") ]     -- XF86AudioPlay

  -- For Programmers Dvorak
  ++ mapWS modifier (windows . S.greedyView) wsWithKeys
  ++ mapWS (modifier .|. shiftMask) (\ i -> (windows $ S.shift i) >> (windows $ S.greedyView i)) wsWithKeys
  where
    wsWithKeys = zip myWorkspaces workspaceKeys
    toggleWS   = windows $ S.view =<< S.tag . head . filter ((\ x -> x /= "NSP") . S.tag) . S.hidden

    mapWS m a ws = map (\ (i, k) -> ((m, k), a i)) ws
