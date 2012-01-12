#!/usr/bin/env python

from gimpfu import *

class Add:
  def eval(self, a, b):
    return a + b

class Set:
  def eval(self, a, b):
    return b

class ResizeBoundary:
  def resize(self, layer, w, h):
    layer.resize(w, h, 0, 0)

class ResizeLayer:
  def resize(self, layer, w, h):
    layer.scale(w, h, True)

operators = [Add(), Set()]
resizers  = [ResizeBoundary(), ResizeLayer()]

def python_resize_all_layers(img, drawable, resize, operator, width, height):
  img.undo_group_start()

  try:
    op      = operators[operator]
    resizer = resizers[resize]

    for layer in img.layers:
      nw = op.eval(layer.width, width)
      nh = op.eval(layer.height, height)

      resizer.resize(layer, nw, nh)
  finally:
    img.undo_group_end()

  gimp.displays_flush()

register( "python_fu_resize_all_layers"
        , "Resize all layers."
        , "Resize all layers."
        , "Daniel Oom"
        , "Daniel Oom"
        , "2011"
        , "<Image>/Filters/Sprite Sheet/Resize All..."
        , "*"
        , [ (PF_OPTION, "resize", "Resize", 0, ["Boundary", "Layer"])
          , (PF_OPTION, "operator", "Operator", 0, ["Add", "Set"])
          , (PF_INT, "width", "Width", 0)
          , (PF_INT, "height", "Height", 0) ]
        , []
        , python_resize_all_layers)

main()
