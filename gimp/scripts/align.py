#!/usr/bin/env python

from sprites import *
from gimpfu import *

def python_layout_tiles(img, drawable, alignH = 0, alignV = 0, layout = "horizontal", limit = 0):
  img.undo_group_start()

  try:
    if layout == "horizontal":
      d = Layout.HORIZONTAL
    elif layout == "vertical":
      d = Layout.VERTICAL

    align  = Alignment(alignH, alignV)
    layout = Layout(d, limit)

    w, h = align_and_layout(img, align, layout)

    pdb.gimp_image_grid_set_spacing(img, w, h)

  finally:
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
        , [ (PF_OPTION, "alignH", "Horzontal alignment", 0, ["Left", "Center", "Right"])
          , (PF_OPTION, "alignV", "Vertical alignment", 0, ["Top", "Middle", "Bottom"])
          , (PF_RADIO, "layout", "Tile layout", "horizontal", (("Horizontal", "horizontal"), ("Vertical", "vertical")))
          , (PF_INT, "limit", "Limit", 0) ]
        , []
        , python_layout_tiles)

main()
