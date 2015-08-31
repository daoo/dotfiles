from layout import Layout
from alignment import Alignment
from gimpfu import *

def calculate_tile_size(img):
  w = 0
  h = 0
  for layer in img.layers:
    w = max(w, layer.width)
    h = max(h, layer.height)

  return w, h

def align_and_layout(img, align, layout):
  # tile width and height
  tw, th = calculate_tile_size(img)

  layout.reset()
  for layer in img.layers:
    # tile index
    tx, ty = layout.pop()

    # tile offset relative to (0, 0)
    ax, ay = tx * tw, ty * th

    # new layer offsets
    ox, oy = align.get_offsets(ax, ay, tw, th,
                               layer.offsets[0], layer.offsets[1],
                               layer.width, layer.height)

    layer.set_offsets(ax + ox, ay + oy)

  rows, cols = layout.get_size(len(img.layers))
  img.resize(rows * tw, cols * th, 0, 0)

  return tw, th

