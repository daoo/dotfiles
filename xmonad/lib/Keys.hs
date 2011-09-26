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
keysToAdd cfg modMask = fromList $
  [ ((modMask, xK_u), withFocused toggleBorder)
  , ((modMask, xK_o), toggleWS)
  , ((modMask, xK_d), goToSelected defaultGSConfig)

  -- Dynamic Workspaces
  , ((modMask, xK_y), removeWorkspace)
  , ((modMask, xK_f), selectWorkspace myXPConfig)
  , ((modMask, xK_g), withWorkspace myXPConfig (windows . S.shift))

  -- Terminals and stuff
  , ((modMask, xK_p), shellPrompt myXPConfig)
  , ((modMask, xK_dollar), scratchpadSpawnActionTerminal $ term cfg)
  , ((modMask, xK_asciitilde), scratchpadSpawnActionTerminal $ term cfg)
  , ((modMask, xK_grave), scratchpadSpawnActionTerminal $ term cfg)
  , ((modMask, xK_Return), spawn $ term cfg)

  -- Software
  , ((modMask, xK_x), spawn $ browser cfg)
  , ((modMask, xK_b), spawn $ editor cfg)
  , ((modMask, xK_m), spawn $ lock cfg)
  
  -- Multimedia keys
  , ((0, 0x1008ff30), spawn "mpc prev > /dev/null") -- XF86Favorites
  , ((0, 0x1008ff19), spawn "mpc next > /dev/null") -- XF86Email
  , ((0, 0x1008ff12), spawn "pa-mute")              -- XF86AudioMute
  , ((0, 0x1008ff14), spawn "mpd-play-pause") ]     -- XF86AudioPlay

  -- For Programmers Dvorak
  ++ mapWS modMask (windows . S.greedyView) ws
  ++ mapWS (modMask .|. shiftMask) (\ i -> (windows $ S.shift i) >> (windows $ S.greedyView i)) ws
  where
    ws        = zip myWorkspaces workspaceKeys
    toggleWS  = windows $ S.view =<< S.tag . head . filter ((\ x -> x /= "NSP") . S.tag) . S.hidden

mapWS m a ws = map (\ (i, k) -> ((m, k), a i)) ws
