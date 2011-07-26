#!/usr/bin/env python

from sprites import *
from math import *
from gimpfu import *

class Limit:
  NONE  = 0
  EVEN  = 1
  VALUE = 2

def get_offset(tile_size, layer_size, align):
  if align == 0:
    return 0
  elif align == 1:
    return tile_size / 2 - layer_size - 2
  elif align == 2:
    return tile_size - layer_size

  return 0

def align_tile(layer, h, v):


def python_layout_tiles(img, drawable, alignH = 0, alignV = 0):
  img.undo_group_start()

  # Find largest layer and use it as tile size
  tile_width, tile_height = find_largest(img.layers)

  # Setup the grid
  pdb.gimp_image_grid_set_spacing(img, tile_width, tile_height)

  # Resize the image
  img.resize(img_width, img_height, 0, 0)

  # Reposition the content
  x = 0
  y = 0
  for layer in img.layers:
    tile_x = tile_width * x
    tile_y = tile_height * y

    offset_x = tile_x + get_offset(tile_width, layer.width, alignH)
    offset_y = tile_y + get_offset(tile_height, layer.height, alignV)

    layer.set_offsets(offset_x, offset_y)

    if layout == Layout.HORIZONTAL:
      x += 1
      if x >= tile_count_layout:
        x = 0
        y += 1
    elif layout == Layout.VERTICAL:
      y += 1
      if y >= tile_count_layout:
        y = 0
        x += 1

  img.undo_group_end()
  gimp.displays_flush()

register("python_fu_layout_tiles"
        , "Automatic layout tool for sprite sheets."
        , "Automatic layout tool for sprite sheets."
        , "Daniel Oom"
        , "Daniel Oom"
        , "2011"
        , "<Image>/Filters/Sprite Sheet/Layout Tiles..."
        , "*"
        , [ (PF_OPTION, "alignH", "Horzontal alignment", 0, ["Left", "Center", "Right", "Keep"])
          , (PF_OPTION, "alignV", "Vertical alignment" , 0, ["Top", "Center", "Bottom", "Keep"])
          , (PF_RADIO , "layout", "Tile layout", "horizontal", (("Horizontal", "horizontal"), ("Vertical", "vertical")))
          , (PF_OPTION, "limit", "Limit direction", 0, ["No limit", "Even", "Value"])
          , (PF_INT   , "limitCount", "Limit value", 0) ]
        , []
        , python_fu_layout_tiles)

main()
