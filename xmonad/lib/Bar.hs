module Bar where

-- Bars
data BarAlign = AlignLeft | AlignCenter | AlignRight

instance Show BarAlign where
  show AlignCenter = "c"
  show AlignLeft   = "l"
  show AlignRight  = "r"

data Bar = Bar
  { barAlign :: BarAlign
  , barBg :: String
  , barFg :: String
  , barFont :: String
  , barHeight :: Int
  , barWidth :: Int
  , barX :: Int, barY :: Int
  } deriving Show

barToString :: Bar -> String
barToString bar = unwords
  [ "-fn", show $ barFont bar
  , "-fg", show $ barFg bar
  , "-bg", show $ barBg bar
  , "-x", show $ barX bar
  , "-y", show $ barY bar
  , "-w", show $ barWidth bar
  , "-h", show $ barHeight bar
  , "-ta", show $ barAlign bar ]
