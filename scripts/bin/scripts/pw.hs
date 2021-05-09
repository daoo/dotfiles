#!/usr/bin/env runhaskell
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad
import Control.Monad.Except
import Data.Bifunctor
import Data.List
import Data.Map (Map)
import Data.Maybe (fromMaybe, listToMaybe, catMaybes)
import Data.Monoid
import Data.Text (Text)
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Process
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Options.Applicative as Opt

-- $setup
-- >>> import Data.Char (isSpace)

-- |Split list at the elements where the predicate is true.
--
-- Note that it is the same as 'words' but not 'lines' because consecutive
-- elements where the predicate matches is treated as a single separator while
-- 'lines' treats them as multiple.
--
-- prop> splitWhile isSpace xs == words xs
-- prop> splitWhile (=='\n') xs == filter (not . null) (lines xs)
--
-- >>> splitWhile (==' ') "a b  c   d"
-- ["a","b","c","d"]
splitWhile :: (a -> Bool) -> [a] -> [[a]]
splitWhile f xs = case dropWhile f xs of
  [] -> []
  xs' -> uncurry (:) (second (splitWhile f) (break f xs'))

xselCopy :: Text -> ExceptT String IO ()
xselCopy clip = do
  exit <- lift $ do
    let xselproc = (proc "xsel" ["--input", "--clipboard"]) {std_in = CreatePipe, std_err = Inherit}
    (Just hin, _, _, h) <- createProcess xselproc
    T.hPutStr hin clip
    hClose hin
    waitForProcess h
  case exit of
    ExitSuccess -> pure ()
    ExitFailure n -> throwError ("xsel failed with exit code " <> show n)

gpgDecrypt :: Handle -> ExceptT String IO Text
gpgDecrypt handle = do
  (out, exit) <- lift $ do
    let gpgproc = (proc "gpg2" ["--batch", "--quiet", "--decrypt"]) {std_in = UseHandle handle, std_out = CreatePipe, std_err = Inherit}
    (_, Just hout, _, h) <- createProcess gpgproc
    out <- T.hGetContents hout
    hClose hout
    (,) out <$> waitForProcess h
  case exit of
    ExitSuccess -> pure out
    ExitFailure n -> throwError ("gpg2 failed with exit code " <> show n)

data Password = Password
  { identifier :: Text
  , password :: Text
  , fields :: Map Text Text
  } deriving Show

parsePasswords :: Monad m => Text -> ExceptT String m [Password]
parsePasswords = mapM parsePassword . splitWhile T.null . T.lines

parsePassword :: Monad m => [Text] -> ExceptT String m Password
parsePassword (ident:pw:fs) = Password ident pw <$> parseFields fs
parsePassword _ = throwError "failed to parse password"

parseFields :: Monad m => [Text] -> ExceptT String m (Map Text Text)
parseFields = fmap M.fromList . mapM parseField

parseField :: Monad m => Text -> ExceptT String m (Text, Text)
parseField text
  | T.null k = throwError "failed to parse field"
  | otherwise = pure (T.strip k, T.strip (T.drop 1 v))
  where
    (k, v) = T.break (==':') text

data Opts = Opts FilePath Mode

data Action = Copy Int | Print | Show

data Mode = List | With Text Action | Csv

opts :: FilePath -> Opt.ParserInfo Opts
opts defpath = Opt.info (Opt.helper <*> parse) (Opt.fullDesc <> header)
  where
    header = Opt.header "PW, password manager for the shell using GnuPG."

    parse = Opts <$> flagPath <*> commandMode

    commandMode :: Opt.Parser Mode
    commandMode = Opt.subparser $ optList <> optCopy <> optPrint <> optShow <> optCsv

    optCommand name parser desc =
      Opt.command name (Opt.info (Opt.helper <*> parser) (Opt.progDesc desc))

    optList = optCommand "list"
      (pure List)
      "List all availible password identifiers"

    optCsv = optCommand "csv"
      (pure Csv)
      "Export to csv"

    optCopy = optCommand "copy"
      (With <$> argumentIdent <*> (Copy <$> flagClear))
      "Copy the password for IDENTIFIER to clipboard"

    optPrint = optCommand "print"
      (With <$> argumentIdent <*> pure Print)
      "Print the password for IDENTIFIER to standard output"

    optShow = optCommand "show"
      (With <$> argumentIdent <*> pure Show)
      "Show the extra password information for IDENTIFIER"

    argumentIdent :: Opt.Parser Text
    argumentIdent = T.pack <$> Opt.argument Opt.str (Opt.metavar "IDENTIFIER")

    flagClear :: Opt.Parser Int
    flagClear = Opt.option Opt.auto $
      Opt.showDefault <>
      Opt.long "clear-time" <>
      Opt.short 'c' <>
      Opt.metavar "SECONDS" <>
      Opt.value 45 <>
      Opt.help "Timeout after which the clipboard is cleared"

    flagPath :: Opt.Parser FilePath
    flagPath = Opt.strOption $
      Opt.showDefault <>
      Opt.long "file" <>
      Opt.short 'f' <>
      Opt.metavar "FILENAME" <>
      Opt.value defpath <>
      Opt.help "Path to the GnuPG encrypted passwords file"

main :: IO ()
main = defpath >>= Opt.execParser . opts >>= program
  where
    defpath = (</> ".local/share/pw/passwords.gpg") <$> getEnv "HOME"

program :: Opts -> IO ()
program (Opts path mode) = handleExceptT $ case mode of
  List -> readPws >>= listPws
  Csv -> readPws >>= csvPws
  With ident action
    | T.null ident -> throwError "No password identifier provided."
    | otherwise -> readPws >>= findPw ident >>= actionPw action
  where
    openPws :: IO Handle
    openPws = case path of
      "-" -> return stdin
      _ -> openFile path ReadMode

    readPws :: ExceptT String IO [Password]
    readPws = lift openPws >>= gpgDecrypt >>= parsePasswords

    listPws :: [Password] -> ExceptT String IO ()
    listPws = mapM_ (lift . T.putStrLn . identifier)

    csvPws :: [Password] -> ExceptT String IO ()
    csvPws = mapM_ (lift . T.putStrLn . fmt)
      where
        fmt pw = T.intercalate "," $ map quote
          [ "ungrouped"
          , identifier pw
          , maybe mempty snd user
          , escape $ password pw
          , url
          , escape notes
          ]
          where
            quote s = T.pack "\"" `T.append` s `T.append` T.pack "\""
            escape s = T.replace "\"" "\"\"" s

            user :: Maybe (Text, Text)
            user = listToMaybe $ catMaybes user'
            user' :: [Maybe (Text, Text)]
            user' = map (\k -> (k,) <$> (fields pw M.!? k)) ["userid", "user", "username", "email", "name"]

            url = fromMaybe mempty $ fields pw M.!? "url"

            fmtnote (k, v) = k `T.append` ": " `T.append` v
            notes :: Text
            notes = T.intercalate "\n" $ map fmtnote $ M.assocs (notes' user)
            notes' Nothing = notes''
            notes' (Just (k, _)) = M.delete k notes''
            notes'' = M.delete "url" $ fields pw

    actionPw :: Action -> Password -> ExceptT String IO ()
    actionPw = \case
      Copy sec -> (>> clearAfter sec) . xselCopy . password
      Print -> lift . T.putStr . password
      Show -> showPw

    showPw :: Password -> ExceptT String IO ()
    showPw pw = lift $ do
      T.putStrLn (identifier pw)
      T.putStrLn "******"
      let put (k, v) = T.putStr k >> T.putStr ": " >> T.putStrLn v
      mapM_ put (M.assocs (fields pw))

    findPw :: Text -> [Password] -> ExceptT String IO Password
    findPw ident = maybe handleError pure . find ((==ident) . identifier)
      where
        handleError = throwError ("Password \"" <> T.unpack ident <> "\" not found.")

    clearAfter :: Int -> ExceptT String IO ()
    clearAfter 0   = pure ()
    clearAfter sec = void $ lift $ spawnCommand ("sleep " ++ show sec ++ " && xsel --clear --clipboard")

    handleExceptT :: ExceptT String IO a -> IO a
    handleExceptT = runExceptT >=> either handleError pure
      where
        handleError msg = hPutStrLn stderr msg >> exitFailure
