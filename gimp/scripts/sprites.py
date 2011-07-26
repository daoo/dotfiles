#!/usr/bin/env python

#from gimpfu import *

class Layout:
  VERTICAL   = 0
  HORIZONTAL = 1

  direction = VERTICAL
  limit     = 0

  __current = 0, 0

  def __init__(self, direction, limit):
    self.direction = direction
    self.limit     = limit
  
  def pop(self):
    old  = self.__current
    x, y = self.__current

    if self.direction == Layout.HORIZONTAL:
      q, r = divmod(x + 1, self.limit)

      x  = r
      y += q

    elif self.direction == Layout.VERTICAL:
      q, r = divmod(y + 1, self.limit)

      x += q
      y  = r

    self.__current = x, y

    return old

class Alignment:
  FIRST  = 0 # Top or left
  SECOND = 1 # Middle or center
  THIRD  = 2 # Bottom or right
  KEEP   = 3

  v_align = FIRST
  h_align = FIRST

  def __init__(self, h_align, v_align):
    self.v_align = v_align
    self.h_align = h_align

  def __get_offset(align, tile_size, layer_size, current):
    if align == FIRST:
      return 0
    elif align == SECOND:
      return tile_size / 2 - layer_size / 2
    elif align == THIRD:
      return tile_size - layer_size
    else:
      return current

  def get_offsets(self, tile_size, layer):
    x = __get_offset(v_align, tile_size.width, layer.width, tile_size.width % layer.offsets[0])
    y = __get_offset(h_align, tile_size.height, layer.height, tile_size.height % layer.offsets[1])

    return x, y

def find_bounding(layers):
  w = 0
  h = 0
  for layer in layers:
    w = max(w, layer.width)
    h = max(h, layer.height)

  return w, h

class SpriteSheet:
  # TODO: Spacing

  def __init__(self, image):
    self.image = image

  def clean_up(self):
    # Autocrop and rename all layers
    i = 1
    for layer in self.image.layers:
      layer.name = "Sprite " + str(i)
      
      # For some reason, autocrop always executes on the currently selected layer
      self.image.active_layer = layer
      pdb.plug_in_autocrop_layer(self.image, layer)

      i += 1

  def align_and_layout(self, align, layout):
    tile_width, tile_height = find_bounding(self.image.layers)

    if align.v_align == Alignment.KEEP:
      new_width = 0
      for layer in self.image.layers:
        tx        = layer.offsets[0] % tile_width
        new_width = max(new_width, tx + layer.width)
      
      tile_width = new_width

    if align.v_align == Alignment.KEEP:
      new_height = 0
      for layer in self.image.layers:
        ty         = layer.offsets[0] % tile_height
        new_height = max(new_height, ty + layer.height)
 
      tile_height = new_height

    for layer in self.image.layers:
      tx, ty = layout.pop()
      ox, oy = align.get_offsets(tile_size, layer)

      layer.set_offsets(tx * tile_width + ox, ty * tile_height + oy)

class TileSize:
  width = 0
  height = 0

  def __init__(self, width, height):
    self.width  = width
    self.height = height
