#!/usr/bin/env python

from gimpfu import *

# TODO: More transforms

def python_transform_layers(img, drawable):
  img.undo_group_start()

  try:
    for layer in img.layers:
      pdb.gimp_item_transform_flip_simple(layer, 0, True, 0)     
  finally:
    img.undo_group_end()

  gimp.displays_flush()

register("python_fu_transform_layers"
        , "Transform all layers in some way"
        , "Transform all layers in some way"
        , "Daniel Oom"
        , "Daniel Oom"
        , "2011"
        , "<Image>/Filters/Sprite Sheet/Transform Layers..."
        , "*"
        , []
        , []
        , python_transform_layers)

main()
