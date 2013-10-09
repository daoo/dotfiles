{-# LANGUAGE LambdaCase #-}
module Main where

-- TODO: Create directories
-- TODO: Copy album art

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

-- TODO: Improve performance
-- Look into posix-paths, problem is number of file operations. In C with
-- dirent we don't have to make another IO call for finding out if we're
-- dealing with a directory or not.
getRecursiveAbsolute :: FilePath -> IO [FilePath]
getRecursiveAbsolute topdir = do
  names <- getDirectoryContents topdir
  fmap concat $ forM names $ \case
    ('.' : [])       -> return []
    ('.' : '.' : []) -> return []
    name             -> do
      let path = topdir </> name
      isdir <- doesDirectoryExist path
      if isdir
        then getRecursiveAbsolute path
        else return [path]

copy :: FilePath -> FilePath -> [FilePath] -> IO ()
copy from to = go S.empty
  where
    go :: S.Set FilePath -> [FilePath] -> IO ()
    go _    []           = return ()
    go dirs (file:files) = do
      dirs' <- if S.member dir dirs
        then return dirs
        else fnewdir >> return (S.insert dir dirs)
      fcpy
      go dirs' files
      where
        fnewdir = putStrLn dir
        fcpy = putStrLn $ "cp \"" ++ (from </> file) ++ "\" \"" ++ (to </> file) ++ "\"" -- copyFile (from </> x) (to </> x)

        dir = takeDirectory file

        fromdir = from </> dir
        todir   = to </> dir

askIfRemove :: FilePath -> IO Bool
askIfRemove file = do
  putStr ("Remove " ++ file ++ " [y/N]? ") >> (== "y") `fmap` getLine

main :: IO ()
main = getArgs >>= \case
  [playlist, prefix, out] -> do
    checkFile playlist
    checkDir prefix
    checkDir out
    inplaylist <- lines <$> readFile playlist
    intarget   <- getRecursiveAbsolute out
    let a = S.fromList $ map (makeValid . makeRelative prefix) inplaylist
        b = S.fromList $ map (makeValid . makeRelative out) intarget
        cpy = a S.\\ b
        del = b S.\\ a
     in copy prefix out (S.toList cpy)
     >> (filterM askIfRemove (S.toList del) >>= print)

  ["-h"]     -> hPutStrLn stdout help
  ["--help"] -> hPutStrLn stdout help
  _          -> hPutStrLn stderr help

help :: String
help = "Usage: syncer PLAYLIST PATHPREFIX OUTDIR"
