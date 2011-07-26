def get_offset(align, tile_size, layer_size, current):
  if align == Alignment.FIRST:
    return 0
  elif align == Alignment.SECOND:
    return tile_size / 2 - layer_size / 2
  elif align == Alignment.THIRD:
    return tile_size - layer_size
  else:
    return current

class Alignment:
  FIRST  = 0 # Top or left
  SECOND = 1 # Middle or center
  THIRD  = 2 # Bottom or right
  KEEP   = 3

  v_align = FIRST
  h_align = FIRST

  def __init__(self, h_align, v_align):
    self.h_align = h_align
    self.v_align = v_align

  def get_offsets(self, tile_width, tile_height, layer):
    x = get_offset(self.h_align, tile_width, layer.width, layer.offsets[0] % tile_width)
    y = get_offset(self.v_align, tile_height, layer.height, layer.offsets[1] % tile_height)

    return x, y

