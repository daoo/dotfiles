#!/usr/bin/env python

from gimpfu import *

class Layout:
  VERTICAL   = 0
  HORIZONTAL = 1

  direction = VERTICAL
  limit     = 0
  layout    = []

  def __init__(self, direction, limit):
    self.direction = direction
    self.limit     = limit

    self.layout = [(0, 0)] # Always start at (0, 0)

    if direction == Layout.HORIZONTAL:
      x = 1
      y = 0
      for i in range(1, tile_count):
        layout.append((x, y))

        x += 1
        if x > limit:
          x  = 0
          y += 1
    elif direction == Layout.VERTICAL:
      x = 0
      y = 1
      for i in range(1, tile_count):
        layout.append((x, y))

        y += 1
        if x > limit:
          x += 1
          y  = 0

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

  def __get_offset(self, align, tile_size, layer_size, current):
    if align == FIRST:
      return 0
    elif align == SECOND:
      return tile_size / 2 - layer_size / 2
    elif align == THIRD:
      return tile_size - layer_size
    elif align == KEEP:
      return current % tile_size

  def get_x_offset(self, tile_size, layer_size, current):
    return __get_offset(self, h_align, layer_size, current)

  def get_y_offset(self, tile_size, layer_size, current):
    return __get_offset(self, v_align, layer_size, current)

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
        tx         = layer.offsets[0] % tile_height
        new_height = max(new_height, tx + layer.height)
 
      tile_height = new_height

    for (x, y) in layout.layout:
      print("ASD")
    
