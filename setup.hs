import System.Directory
import System.FilePath
import System.Posix.Files

data Environment = Environment { sourceDir :: FilePath
                               , targetDir :: FilePath
                               , firefoxDefaultProfile :: FilePath }

data Action = Link FilePath FilePath
            | LinkAll FilePath FilePath
            | MakeDir FilePath
            | Lambda (Environment -> IO ())
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
    firefox :: Environment -> IO ()
    firefox env@(Environment fromDir outDir) = do
      f <- getFireFoxDefaultProfilePath env
      case f of
        Nothing   -> ()
        Just path -> do
          let target = f </> "searchplugins"
          e <- directoryExists target
          if e
            then ()
            else createSymbolicLink (fromDir </> target)

execute :: Environment -> Action -> IO ()
execute (Environment fromDir outDir) (Link targetFile linkName) = createSymbolicLink (fromDir </> targetFile) (outDir </> linkName)
execute (Environment _ outDir) (MakeDir path)                   = createDirectoryIfMissing True $ outDir </> path
execute env (Lambda f)                                          = f env
execute _ _                                                     = ()

main :: IO ()
main = do
  let env = Environment "TODO" "TODO"
  mapM (execute env) actions

getFireFoxDefaultProfilePath :: Environment -> IO (Maybe FilePath)
getFireFoxDefaultProfilePath = Nothing

