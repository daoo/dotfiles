#!/usr/bin/env python

from gimpfu import *

# TODO: Column or row major?

def python_split(img, drawable, width, height):
  img.undo_group_start()

  try:
    if width > 0 and height > 0:
      i = 0

      # Goes rows first
      y = 0
      while y < drawable.height:
        x = 0
        while x < drawable.width:
          new = drawable.copy()
          img.add_layer(new, len(img.layers)) # Add the layer at the end

          new.resize(width, height, -x, -y) # Move to original content in the "opposite" direction
          new.set_offsets(x, y)

          new.name = "Split " + str(i)
          i += 1

          x += width

        y += height
      
      img.remove_layer(drawable)
  finally:
    img.undo_group_end()

  gimp.displays_flush()

register("python_fu_split"
        , "Split a layer into new layers using a grid pattern."
        , "Split a layer into new layers using a grid pattern."
        , "Daniel Oom"
        , "Daniel Oom"
        , "2011"
        , "<Image>/Filters/Sprite Sheet/Split..."
        , "*"
        , [ (PF_INT, "width", "Width", 0)
          , (PF_INT, "height", "Height", 0) ]
        , []
        , python_split)

main()
