module Prompt where

import XMonad
import XMonad.Prompt
import XMonad.Prompt.Shell hiding (shellPrompt)

shellPrompt :: XPConfig -> X ()
shellPrompt c = mkXPrompt Shell c (getShellCompl cmds) spawn
  where
    cmds = [ "firefox", "gvim" ]
