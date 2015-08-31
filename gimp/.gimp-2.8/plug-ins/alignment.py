def get_offset(align, tile_size, layer_offset, layer_size):
  if align == Alignment.KEEP:
    return layer_offset
  elif align == Alignment.FIRST:
    return 0
  elif align == Alignment.SECOND:
    return tile_size / 2 - layer_size / 2
  elif align == Alignment.THIRD:
    return tile_size - layer_size

  return 0

class Alignment:
  KEEP   = 0
  FIRST  = 1 # Top or left
  SECOND = 2 # Middle or center
  THIRD  = 3 # Bottom or right

  v_align = FIRST
  h_align = FIRST

  def __init__(self, h_align, v_align):
    self.h_align = h_align
    self.v_align = v_align

  # Get offsets for the layer according to the alignment rules
  def get_offsets(self, tx, ty, tw, th, lx, ly, lw, lh):
    # tx, ty -- tile offset relative to (0, 0)
    # tw, th -- tile size
    # lx, ly -- layer offset relative to (0, 0)
    # lw, lh -- layer size

    x = get_offset(self.h_align, tw, lx % tw, lw)
    y = get_offset(self.v_align, th, ly % th, lh)

    return x, y

