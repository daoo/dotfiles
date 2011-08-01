def get_offset(align, tile_size, layer_size):
  if align == Alignment.FIRST:
    return 0
  elif align == Alignment.SECOND:
    return tile_size / 2 - layer_size / 2
  elif align == Alignment.THIRD:
    return tile_size - layer_size

  return 0

class Alignment:
  FIRST  = 0 # Top or left
  SECOND = 1 # Middle or center
  THIRD  = 2 # Bottom or right

  v_align = FIRST
  h_align = FIRST

  def __init__(self, h_align, v_align):
    self.h_align = h_align
    self.v_align = v_align

  def get_offsets(self, tile_width, tile_height, layer):
    x = get_offset(self.h_align, tile_width, layer.width)
    y = get_offset(self.v_align, tile_height, layer.height)

    return x, y
