{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Applicative
import Control.Monad
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import qualified Data.Set as S

notWhenM :: Monad m => m Bool -> m () -> m ()
notWhenM b m = b >>= ((`when` m) . not)

checkFile :: FilePath -> IO ()
checkFile path = notWhenM (doesFileExist path) (fail $ path ++ " is not a file")

checkDir :: FilePath -> IO ()
checkDir path = notWhenM (doesDirectoryExist path) (fail $ path ++ " is not a directory")

getDirectoryContents' :: FilePath -> IO [FilePath]
getDirectoryContents' = fmap (filter (`notElem` [".", ".."])) . getDirectoryContents

getRecursiveAbsolute :: FilePath -> IO [FilePath]
getRecursiveAbsolute topdir = do
  names <- getDirectoryContents' topdir
  fmap concat $ forM names $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveAbsolute path
      else return [path]

copy :: FilePath -> FilePath -> [FilePath] -> IO ()
copy from to = go S.empty
  where
    go dirs (x:xs) = copyFile (from </> x) (to </> x)
      -- TODO: Create directories
      -- TODO: Copy album art

-- Let A be playlist
-- Let B be files in target
-- Copy A \ B to target
-- Remove B \ A from B

main :: IO ()
main = getArgs >>= \case
  [playlist, prefix, out] -> do
    checkFile playlist
    checkDir prefix
    checkDir out
    inplaylist <- lines <$> readFile playlist
    intarget   <- getRecursiveAbsolute out
    let a = S.fromList $ map (makeRelative prefix) inplaylist
        b = S.fromList $ map (makeRelative out) intarget
        cpy = a S.\\ b
        del = b S.\\ a
     in do
      mapM_ (\f -> putStrLn $ "cp " ++ (prefix </> f) ++ " " ++ (out </> f)) (S.toList cpy)
      mapM_ (\f -> putStrLn $ "rm " ++ (out </> f)) (S.toList del)

  ["-h"]     -> hPutStrLn stdout help
  ["--help"] -> hPutStrLn stdout help
  _          -> hPutStrLn stderr help

help :: String
help = "Usage: syncer PLAYLIST PATHPREFIX OUTDIR"
