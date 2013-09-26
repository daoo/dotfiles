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
    , "eclipse"
    , "emacs"
    , "firefox"
    , "gimp"
    , "gvim"
    , "keepassx"
    , "libreoffice"
    , "lxappearance"
    , "minecraft"
    , "pavucontrol"
    , "pidgin"
    , "skype"
    , "tuxguitar"
    , "urxvt"
    , "zathura"
    ]
