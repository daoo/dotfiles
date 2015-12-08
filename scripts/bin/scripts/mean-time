#!/usr/bin/env runhaskell
{-# LANGUAGE LambdaCase #-}

import Control.Monad
import Data.Ratio
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Environment
import System.IO
import System.Process
import Text.Printf

formatTime :: Double -> String
formatTime s
  | s < 1 = printf "%.3fms" (s * 1000.0)
  | otherwise = printf "%.3fs" s

getTime :: IO Double
getTime = (fromRational . toRational) <$> getPOSIXTime

timeIt :: String -> [String] -> IO Double
timeIt path args = do
  t1 <- getTime
  callProcess path args
  t2 <- getTime
  return (t2 - t1)

meanSdev :: [Double] -> (Double, Double)
meanSdev xs = (ex, sdev)
  where
    ex = mean xs
    ex2 = mean (map (\x -> x*x) xs)
    sdev = sqrt (ex2 - ex*ex)

    mean xs = sum xs / fromIntegral (length xs)

prog :: String -> [String] -> IO ()
prog path args = do
  -- Run once to update file system caches
  callProcess path args
  let measurements = 20
  times <- replicateM measurements (timeIt path args)
  let (mean, sdev) = meanSdev times
  printf "mean=%s, sdev=%s\n" (formatTime mean) (formatTime sdev)

main :: IO ()
main = getArgs >>= \case
  path:args -> prog path args
  _ -> hPutStrLn stderr "Usage: adv-time.hs PROGRAM [ARGS]"
