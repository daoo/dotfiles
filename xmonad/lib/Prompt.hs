module Prompt (launchPrompt) where

import XMonad
import XMonad.Prompt
import XMonad.Prompt.Input

launchPrompt :: XPConfig -> X ()
launchPrompt c = inputPromptWithCompl c "Run" (mkComplFunFromList cmds) ?+ spawn
  where
    cmds = [ "firefox", "gvim" ]
