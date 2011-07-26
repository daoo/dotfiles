from layout import Layout
from alignment import Alignment
from gimpfu import *

def calculate_tile_size(img, keep_x = False, keep_y = False):
  w = 0
  h = 0
  for layer in img.layers:
    w = max(w, layer.width)
    h = max(h, layer.height)

  if keep_x:
    nw = 0
    for layer in img.layers:
      tx = layer.offsets[0] % w
      nw = max(nw, tx + layer.width)

    w = nw

  if keep_y:
    nh = 0
    for layer in img.layers:
      ty = layer.offsets[0] % h
      nh = max(nh, ty + layer.height)

    h = nh

  return w, h

def clean_up(img):
  # Autocrop and rename all layers
  i = 1
  for layer in img.layers:
    layer.name = "Sprite " + str(i)

    # For some reason, autocrop always executes on the currently selected layer
    img.active_layer = layer
    pdb.plug_in_autocrop_layer(img, layer)

    i += 1

def align_and_layout(img, align, layout):
  tw, th = calculate_tile_size(
      img,
      align.h_align == Alignment.KEEP,
      align.v_align == Alignment.KEEP)

  layout.reset()
  for layer in img.layers:
    tx, ty = layout.pop()
    ox, oy = align.get_offsets(tw, th, layer)

    layer.set_offsets(tx * tw + ox, ty * th + oy)

  rows, cols = layout.get_size(len(img.layers))
  img.resize(rows * tw, cols * th, 0, 0)

  return tw, th

