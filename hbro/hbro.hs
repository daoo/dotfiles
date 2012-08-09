{-# LANGUAGE DoRec #-}
module Main where

-- {{{ Imports
import qualified Hbro.Bookmarks as Bookmarks
import Hbro.Boot
import Hbro.Clipboard
import Hbro.Config
import Hbro.Core
import qualified Hbro.Download as Download
import Hbro.Gui
--import Hbro.Hbro
import qualified Hbro.History as History
import Hbro.Keys
import Hbro.Misc
import qualified Hbro.Prompt as Prompt
import Hbro.Session
--import Hbro.Settings
import Hbro.Socket
import Hbro.StatusBar
import Hbro.Types
import Hbro.Util
import qualified Hbro.WebSettings as WS

import Control.Conditional
import Control.Monad hiding(forM_, mapM_)

import Data.Foldable
import Data.Functor
import Data.Time

import Graphics.UI.Gtk.Abstract.Widget
import Graphics.UI.Gtk.Builder
import Graphics.UI.Gtk.Display.Label
import Graphics.UI.Gtk.Entry.Entry
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.Gdk.GC
import Graphics.UI.Gtk.General.General
import Graphics.UI.Gtk.WebKit.Download
import Graphics.UI.Gtk.WebKit.NetworkRequest
import Graphics.UI.Gtk.WebKit.WebNavigationAction
import Graphics.UI.Gtk.WebKit.WebSettings
import Graphics.UI.Gtk.Windows.Window

import Network.URI

import Prelude hiding(mapM_)

import System.Directory
import System.Environment
import System.Environment.XDG.BaseDir
import System.FilePath
import System.Glib.Attributes
import System.Glib.Signals
-- import System.Posix.Process
import System.Process
-- }}}

-- Main function, expected to call launchHbro.
main :: IO ()
main = hbro myConfig

-- {{{ Configuration structures
-- Any field you don't override will
-- use the defaults defined in Hbro.Types.Config.
myConfig :: Config
myConfig = defaultConfig {
--  mSocketDir        = mySocketDirectory,
    mUIFile           = myUIFile,
    mHomePage         = myHomePage,
    mWebSettings      = myWebSettings,
--  mCommandsList     = myCommandsList,
    mHooks            = myHooks
}

myHooks = defaultHooks {
--  mBackForward     = myBackForward,
    mDownload        = myDownloadHook,
--  mFormResubmitted = myFormResubmitted,
--  mFormSubmitted   = myFormSubmitted,
    mKeyPressed      = emacsKeyHandler myKeys ["M-x"] >=> void . (printInLabel "keys"),
--  mLinkClicked     = myLinkClicked,
    mLoadFinished    = myLoadFinished,
--  mMIMEDisposition = myMIMEDisposition,
--  mNewWindow       = myNewWindowHook,
--  mOtherNavigation = myOtherNavigation,
--  mReload          = myReload,
    mStartUp         = myStartUp
--  mTitleChanged    = myTitleChanged
}
-- }}}

-- {{{ Constant parameters
myHomePage = "https://duckduckgo.com"

myUIFile, myHistoryFile, myBookmarksFile, myDownloadDirectory :: PortableFilePath
myUIFile            = (</> "ui.xml") . mConfiguration
myHistoryFile       = (</> "history") . mData
myBookmarksFile     = (</> "bookmarks") . mData
myDownloadDirectory = (</> "downloads") . mHome
-- }}}

-- {{{ Hooks
myDownloadHook :: URI -> String -> Int -> K ()
myDownloadHook uri filename _size = io $ Download.aria myDownloadDirectory uri filename

myLoadFinished :: K ()
myLoadFinished = History.log myHistoryFile
-- }}}

-- {{{ Keys
-- Note that this example is suited for an azerty keyboard.
myKeys :: KeysList
myKeys = keys `union` defaultKeyBindings
  where
    keys = [
-- Browse
    ("C-<Left>",      goBackList    ["-l", "10"] >>= mapM_ loadURI),
    ("C-<Right>",     goForwardList ["-l", "10"] >>= mapM_ loadURI),
    ("C-g",           Prompt.read "DuckDuckGo search" [] (mapM_ loadURI . parseURIReference . ("https://duckduckgo.com/html?q=" ++) . escapeURIString isAllowedInURI)),
-- Copy/paste
    ("C-y",           withURI       $ io . toClipboard . show),
    ("M-y",           withTitle     $ io . toClipboard),
    ("C-p",           withClipboard $ mapM_ loadURI . parseURIReference),
    ("M-p",           withClipboard $ \uri -> io $ spawn "hbro" ["-u", uri]),
-- Bookmarks
    ("C-d",           Prompt.read "Bookmark with tags:" [] $ Bookmarks.add myBookmarksFile . words),
    ("C-D",           Prompt.read "Bookmark all instances with tag:" [] $ \tags -> do
        (map parseURI <$> sendCommandToAll "GET_URI")
        >>= mapM (mapM_ $ \uri -> (io . Bookmarks.addCustom myBookmarksFile) $ Bookmarks.Entry uri (words tags))
        >> (withURI $ \uri -> (io . void . Bookmarks.addCustom myBookmarksFile) $ Bookmarks.Entry uri (words tags))
    ),
    ("M-d",           io $ Bookmarks.deleteWithTag myBookmarksFile ["-l", "10"]),
    ("C-l",           io (Bookmarks.select        myBookmarksFile ["-l", "10"]) >>= mapM_ loadURI),
    ("C-L",           io (Bookmarks.selectTag     myBookmarksFile ["-l", "10"]) >>= mapM_ (\uris -> mapM (\uri -> io . void $ spawn "hbro" ["-u", (show uri)]) uris)),
--    ("C-q"),           webViewGetUri webView >>= maybe (return ()) (Queue.append),
--    ("M-q"),           \b -> do
--        uri <- Queue.popFront
--        loadURI uri b),

-- History
    ("C-h",           io (History.select myHistoryFile ["-l", "10"]) >>= mapM_ loadURI . (return . (History.mURI) =<<)),

-- Session
    --("M-l"),           loadFromSession ["-l", "10"])
-- Settings
    ("C-j",           WS.toggle webSettingsEnableScripts >>= ((notify 5000 "Javascript disabled") ?? (notify 5000 "Javascript enabled")))
    ]
-- }}}

-- {{{ Web settings
-- Commented out lines correspond to default values.
myWebSettings :: [AttrOp WebSettings]
myWebSettings = [
--  SETTING                                        VALUE
    --webSettingsCursiveFontFamily              := "serif",
    --webSettingsDefaultFontFamily              := "sans-serif",
    --webSettingsFantasyFontFamily              := ,
    webSettingsMonospaceFontFamily              := "consolas",
    --webSettingsSansFontFamily                 := "sans-serif",
    --webSettingsSerifFontFamily                := "serif",
    --webSettingsDefaultFontSize                := ,
    --webSettingsDefaultMonospaceFontSize       := 10,
    --webSettingsMinimumFontSize                := 5,
    --webSettingsMinimumLogicalFontSize         := 5,
    --webSettingsAutoLoadImages                 := True,
    --webSettingsAutoShrinkImages               := True,
    --webSettingsDefaultEncoding                := "iso-8859-1",
    --webSettingsEditingBehavior                := EditingBehaviorWindows,
    --webSettingsEnableCaretBrowsing            := False,
    webSettingsEnableDeveloperExtras            := True,
    --webSettingsEnableHtml5Database              := True,
    --webSettingsEnableHtml5LocalStorage          := True,
    --webSettingsEnableOfflineWebApplicationCache := True,
    webSettingsEnablePlugins                    := False,
    webSettingsEnablePrivateBrowsing            := False, -- Experimental
    webSettingsEnableScripts                    := False,
    --webSettingsEnableSpellChecking              := False,
    webSettingsEnableUniversalAccessFromFileUris := True,
    webSettingsEnableXssAuditor                 := True,
    --webSettingsEnableSiteSpecificQuirks       := False,
    --webSettingsEnableDomPaste                 := False,
    --webSettingsEnableDefaultContextMenu       := True,
    webSettingsEnablePageCache                  := True,
    --webSettingsEnableSpatialNavigation        := False,
    --webSettingsEnforce96Dpi                   := ,
    webSettingsJSCanOpenWindowAuto              := True,
    --webSettingsPrintBackgrounds               := True,
    --webSettingsResizableTextAreas             := True,
    webSettingsSpellCheckingLang                := Just "en_US",
    --webSettingsTabKeyCyclesThroughElements    := True,
    webSettingsUserAgent                        := firefoxUserAgent
    --webSettingsUserStylesheetUri              := Nothing,
    --webSettingsZoomStep                       := 0.1
    ]
-- }}}

-- {{{ Setup
myStartUp :: K ()
myStartUp = do
-- Scroll position in status bar
    setupScrollWidget =<< getObject castToLabel "scroll"

-- Zoom level in status bar
    setupZoomWidget =<< getObject castToLabel "zoom"

-- Load progress in status bar
    setupProgressWidget =<< getObject castToLabel "progress"

-- Current URI in status bar
    setupURIWidget defaultURIColors defaultSecureURIColors =<< getObject castToLabel "uri"

-- Session manager
    --setupSession browser

-- Favicon
    --_ <- on webView iconLoaded $ \uri -> do something

    return ()
-- }}}
