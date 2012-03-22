module Bar where

-- Bars
data BarAlign = AlignLeft | AlignCenter | AlignRight

instance Show BarAlign where
  show AlignLeft   = "l"
  show AlignCenter = "c"
  show AlignRight  = "r"

data Bar = Bar {
  barWidth :: Int, barHeight :: Int,
  barX :: Int, barY :: Int,
  barAlign :: BarAlign,
  barFont :: String,
  barFg :: String, barBg :: String
} deriving (Show)

barToString :: Bar -> String
barToString bar = unwords bar'
  where
    bar' = [ "-fn", show $ barFont bar
           , "-fg", show $ barFg bar
           , "-bg", show $ barBg bar
           , "-x", show $ barX bar
           , "-y", show $ barY bar
           , "-w", show $ barWidth bar
           , "-h", show $ barHeight bar
           , "-ta", show $ barAlign bar ]

