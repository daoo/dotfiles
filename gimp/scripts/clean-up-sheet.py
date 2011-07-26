#!/usr/bin/env python

from gimpfu import *
from sprites import *

def python_clean_up_sheet(img, drawable):
  img.undo_group_start()

  try:
    clean_up(img)
  finally:
    img.undo_group_end()

  gimp.displays_flush()

register("python_fu_clean_up_sheet"
        , "Clean up all layers to make the image more suited as a sprite sheet."
        , "Clean up all layers to make the image more suited as a sprite sheet."
        , "Daniel Oom"
        , "Daniel Oom"
        , "2011"
        , "<Image>/Filters/Sprite Sheet/Clean Up"
        , "*"
        , []
        , []
        , python_clean_up_sheet)

main()
