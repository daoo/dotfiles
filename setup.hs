import System
import System.Directory
import System.FilePath
import System.Posix.Files

data Env = Env FilePath FilePath

data Action = Link FilePath FilePath
            | LinkAll FilePath FilePath
            | MakeDir FilePath
            | Lambda (Env -> IO ())
            | None

type ActionGroup = (String, [Action])

actions :: [ActionGroup]
actions =
  [ ( "VIM"
    , [ Link "vim" ".vim"
      , Link "vim/vimrc" ".vimrc"
      , Link "vim/gvimrc" ".gvimrc"
      , MakeDir ".vim/tmp" ] )
  , ( "Git"
    , [ Link "gitconfig" ".gitconfig"
      , Link "gitignore" ".gitignore" ] )
  , ( "Zsh"
    , [ Link "zsh/zshrc" ".zshrc"
      , Link "zsh" ".zsh" ] )
  , ( "Xmonad"
    , [ Link "xmonad" ".xmonad" ] )
  , ( "Xdefaults"
    , [ Link "Xdefaults" ".Xdefaults" ] )
  , ( "xinitrc"
    , [ Link "xinitrc" ".xinitrc" ] )
  , ( "firefox"
    , [ Lambda firefox ] )
  , ( "bin"
    , [ MakeDir "bin"
      , LinkAll "scripts" "bin" ] )
  ]
  where
    firefox :: Env -> IO ()
    firefox env@(Env sDir tDir) = return ()

execute :: Env -> Action -> IO ()
execute (Env sDir tDir) (Link tFile name) = silentLink (sDir </> tFile) (tDir </> name)
execute (Env sDir tDir) (LinkAll to from) = getDirectoryContents (sDir </> to) >>= mapM_ (\file -> silentLink (sDir </> to </> file) (tDir </> from </> file))
execute (Env _    tDir) (MakeDir path)    = createDirectoryIfMissing True $ tDir </> path
execute env             (Lambda f)        = f env
execute _               _                 = return ()

main :: IO ()
main = do
  let only = ["vim", "zsh"]

  home <- getEnv "HOME"
  let env = Env (home </> "dotfiles") home
  mapM_ (exeGroup env) $ filter (\ (s, _) -> s `elem` only) actions
  where
    exeGroup env (s, as) = putStrLn s >> mapM_ (execute env) as

getFireFoxDefaultProfilePath :: Env -> IO (Maybe FilePath)
getFireFoxDefaultProfilePath (Env sourceDir targetDir) = do
  f <- doesFileExist $ sourceDir </> ".mozilla/firefox"
  if f
    then return Nothing
    else return Nothing

silentLink :: FilePath -> FilePath -> IO ()
silentLink t n = do
  td <- doesDirectoryExist t
  tf <- doesFileExist t
  nd <- doesDirectoryExist n
  nt <- doesFileExist n

  if td || tf -- Target exists
    then do
      if nd || nt -- Link name exists
        then return ()
        else createSymbolicLink t n
    else return ()
