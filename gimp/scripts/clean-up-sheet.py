#!/usr/bin/env python

from math import *
from gimpfu import *

def python_clean_up_sheet(img, drawable):
  img.undo_group_start()

  # Autocrop and rename all layers
  i = 1
  for layer in img.layers:
    layer.name = "Sprite " + str(i)
    
    # For some reason, autocrop always executes on the currently selected layer
    img.active_layer = layer
    pdb.plug_in_autocrop_layer(img, layer)

    i += 1

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
