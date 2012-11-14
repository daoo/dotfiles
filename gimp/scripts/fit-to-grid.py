#!/usr/bin/env python

from gimpfu import *

def python_fit_to_grid(img, drawable):
  img.undo_group_start()

  try:
    tmp1, tmp2 = pdb.gimp_image_grid_get_spacing(img)
    w = int(tmp1)
    h = int(tmp2)

    i = 0
    for layer in img.layers:
      # Note: requires horizontal layout
      grid_x = i * w
      grid_y = 0

      real_x = layer.offsets[0]
      real_y = layer.offsets[1]

      in_x = real_x - grid_x
      in_y = real_y - grid_y

      layer.resize(w, h, in_x, in_y)

      i += 1

  finally:
    img.undo_group_end()

  gimp.displays_flush()

register( "python_fu_fit_to_grid"
        , "Fit all layers into the grid by resizing their layer boundary."
        , "Fit all layers into the grid by resizing their layer boundary."
        , "Daniel Oom"
        , "Daniel Oom"
        , "2012"
        , "<Image>/Filters/Sprite Sheet/Fit to Grid"
        , "*"
        , []
        , []
        , python_fit_to_grid)

main()
