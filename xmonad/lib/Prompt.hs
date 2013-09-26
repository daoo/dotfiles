module Prompt (launchPrompt) where

import XMonad
import XMonad.Prompt
import XMonad.Prompt.Input

launchPrompt :: XPConfig -> X ()
launchPrompt c = inputPromptWithCompl c "Run" (mkComplFunFromList cmds) ?+ spawn
  where
    cmds =
      [ "SpiderOak"
      , "audacity"
      , "blender"
      , "calibre"
      , "coqide"
      , "darktable"
      , "eclipse"
      , "emacs"
      , "firefox"
      , "gimp"
      , "gvim"
      , "keepassx"
      , "libreoffice"
      , "lxappearance"
      , "minecraft"
      , "mirage"
      , "pavucontrol"
      , "pcmanfm"
      , "pidgin"
      , "skype"
      , "tuxguitar"
      , "urxvt"
      , "wirkeshark"
      , "zathura"
      ]
