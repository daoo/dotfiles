from math import *

def inc(a, b, limit):
  a += 1
  if limit > 0:
    if a >= limit:
      a  = 0
      b += 1

  return a, b

class Layout:
  VERTICAL   = 0
  HORIZONTAL = 1

  direction = VERTICAL
  limit     = 0

  __current = 0, 0

  def __init__(self, direction, limit):
    self.direction = direction
    self.limit     = limit

    self.reset()

  def get_size(self, layer_count):
    if self.limit > 0 and layer_count > self.limit:
      tmp = int(ceil(float(layer_count) / float(self.limit)))

      if self.direction == Layout.HORIZONTAL:
        return self.limit, tmp
      elif self.direction == Layout.VERTICAL:
        return tmp, self.limit
    else:
      if self.direction == Layout.HORIZONTAL:
        return layer_count, 1
      elif self.direction == Layout.VERTICAL:
        return 1, layer_count

  def pop(self):
    old_x, old_y = self.__current

    if self.direction == Layout.HORIZONTAL:
      x, y = inc(old_x, old_y, self.limit)
    elif self.direction == Layout.VERTICAL:
      y, x = inc(old_y, old_x, self.limit)

    self.__current = x, y

    return old_x, old_y

  def reset(self):
    self.__current = 0, 0
