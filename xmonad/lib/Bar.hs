module Bar
  ( BarAlign(..)
  , Bar(..)
  , barToString
  ) where

data BarAlign = AlignLeft | AlignCenter | AlignRight

showBarAlign :: BarAlign -> String
showBarAlign AlignCenter = "c"
showBarAlign AlignLeft   = "l"
showBarAlign AlignRight  = "r"

data Bar = Bar
  { barAlign :: BarAlign
  , barBg :: String
  , barFg :: String
  , barFont :: String
  , barHeight :: Int
  , barWidth :: Int
  , barX :: Int, barY :: Int
  }

barToString :: Bar -> String
barToString bar = showString "-fn "  $ shows (barFont bar)
                $ showString " -fg " $ shows (barFg bar)
                $ showString " -bg " $ shows (barBg bar)
                $ showString " -x "  $ shows (barX bar)
                $ showString " -y "  $ shows (barY bar)
                $ showString " -w "  $ shows (barWidth bar)
                $ showString " -h "  $ shows (barHeight bar)
                $ showString " -ta " $ showBarAlign (barAlign bar)
