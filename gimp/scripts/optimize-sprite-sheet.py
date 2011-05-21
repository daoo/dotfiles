#!/usr/bin/env python

# TODO: Padding
# TODO: Max resulting image size
# TODO: Alignment point

import math
from gimpfu import *

def get_offset_x( tile_width, layer_width, alignH, hOffset ):
  if alignH == "left":
    return 0
  elif alignH == "center":
    return tile_width / 2 - layer_width / 2
  elif alignH == "right":
    return tile_width - layer_width
  else:
    return hOffset

def get_offset_y( tile_height, layer_height, alignV, vOffset ):
  if alignV == "left":
    return 0
  elif alignV == "center":
    return layer_height / 2 - tile_height / 2
  elif alignV == "right":
    return tile_height - layer_height
  else:
    return vOffset

def python_optimize_sprite_sheet( img, drawable, alignH = 0, alignV = 0, hOffset = 0, vOffset = 0, layout = "horizontal", even = 0, hTileCount = 0, vTileCount = 0, maxWidth = 0, maxHeight = 0 ):
  img.undo_group_start()

  # Autocrop and rename all layers
  i = 1
  for layer in img.layers:
    layer.name = "Sprite " + str( i )
    img.active_layer = layer
    pdb.plug_in_autocrop_layer( img, layer )

    i = i + 1

  # Find largest layer and use it as tile size
  tile_width  = 0
  tile_height = 0
  for layer in img.layers:
    if layer.width > tile_width:
      tile_width = layer.width
    if layer.height > tile_height:
      tile_height = layer.height

  # Calculate new image size
  tile_count = len( img.layers )
  if even == 1:
    tile_count_layout = math.floor( sqrt( tile_count ) )
  else:
    if hTileCount == 0 and vTileCount == 0:
      tile_count_layout = tile_count
    elif layout == "horizontal":
      tile_count_layout = hTileCount
    elif layout == "vertical":
      tile_count_layout = vTileCount

  if layout == "horizontal":
    img_width  = tile_count_layout * tile_width
    img_height = int( math.ceil( tile_count / tile_count_layout ) ) * tile_height
  elif layout == "vertical":
    img_height = tile_count * tile_height
    img_width  = int( math.ceil( tile_count / tile_count_layout ) ) * tile_width

  # Resize the image and reposition the content
  img.resize( img_width, img_height, 0, 0 )
  if layout == "horizontal":
    x = 0
    y = 0

    for layer in img.layers:
      offset_x = tile_width * x + get_offset_x( tile_width, layer.width, alignH, hOffset )
      offset_y = tile_height * y + get_offset_y( tile_height, layer.height, alignV, vOffset )
      layer.set_offsets( offset_x, offset_y )

      x += 1
      if x > tile_count_layout:
        x = 0
        y += 1

  elif layout == "vertical":
    x = 0
    y = 0

    for layer in img.layers:
      offset_x = tile_width * x + pX + get_offset_x( tile_width, layer.width, alignH, hOffset )
      offset_y = tile_height * y + pY + get_offset_y( tile_height, layer.height, alignV, vOffset )
      layer.set_offsets( offset_x, offset_y )

      y += 1
      if y > tile_count_layout:
        y = 0
        x += 1

  #img.flatten()
  img.undo_group_end()
  gimp.displays_flush()

register( "python_fu_optimize_sprite_sheet"
        , "Optimize the size and arrangement of tiles in a sprite sheet."
        , "Optimize the size and arrangement of tiles in a sprite sheet."
        , "Daniel Oom"
        , "Daniel Oom"
        , "2011"
        , "<Image>/Filters/Animation/Op_timize Sprite Sheet..."
        , "*"
        , [ (PF_OPTION, "alignH", "Horzontal alignment", 0, ["Left", "Center", "Right", "Horizontal Offset"])
          , (PF_OPTION, "alignV", "Vertical alignment" , 0, ["Top", "Center", "Bottom", "Vertical Offset"])
          , (PF_INT   , "hOffset", "Horizontal offset", 0)
          , (PF_INT   , "vOffset", "Vertical offset", 0)
          , (PF_RADIO , "layout", "Tile layout", "horizontal", (("Horizontal", "horizontal"), ("Vertical", "vertical")))
          , (PF_TOGGLE, "even", "Even distribution (if checked, ignores options below)", 0)
          , (PF_INT   , "hTileCount", "Maximum horizontal tile count (0 for infinite)", 0)
          , (PF_INT   , "vTileCount", "Maximum vertical tile count (0 for infinite)", 0) ]
        , []
        , python_optimize_sprite_sheet)

main()
