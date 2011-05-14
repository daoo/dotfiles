#!/usr/bin/env python

import math
from gimpfu import *

def python_optimize_sprite_sheet( img, drawable ):
  img.undo_group_start()

  # Autocrop and rename all layers
  i = 1
  for layer in img.layers:
    layer.name = "Sprite " + str( i )
    img.active_layer = layer
    pdb.plug_in_autocrop_layer( img, layer )

    i = i + 1

  # Find tile size
  tile_width = 0
  tile_height = 0
  for layer in img.layers:
    if layer.width > tile_width:
      tile_width = layer.width
    if layer.height > tile_height:
      tile_height = layer.height

  # Calculate new image size
  tile_count = len( img.layers )
  img_width  = tile_count * tile_width
  img_height = 1 * tile_height # TODO: Currently only one row of tiles

  # Resize the image and reposition the content
  img.resize( img_width, img_height, 0, 0 )
  x = 0
  y = 0
  for layer in img.layers:
    offset_x = tile_width * x
    offset_y = tile_height * y + ( tile_height - layer.height ) # TODO: Optional align to bottom
    layer.set_offsets( offset_x, offset_y )
    x = x + 1

  #img.flatten()
  img.undo_group_end()
  gimp.displays_flush()

register( "python_fu_optimize_sprite_sheet",
          "Optimize the size and arrangement of tiles in a sprite sheet.",
          "Optimize the size and arrangement of tiles in a sprite sheet.",
          "Daniel Oom",
          "Daniel Oom",
          "2011",
          "<Image>/Filters/Animation/Op_timize Sprite Sheet...",
          "*",
          [],
          [],
          python_optimize_sprite_sheet)

main()
