module Main where

import Control.Monad
import Control.Monad.Writer
import Data.List
import System.Directory
import System.FilePath

data Func = File String String FilePath
          | StopModule String
  deriving Show

listFiles :: (String -> Bool) -> FilePath -> IO [FilePath]
listFiles f dir = fmap (map prepend . filter (\a -> f a && notdot a)) $ getDirectoryContents dir
  where
    prepend = (dir </>)
    notdot  = (`notElem` [".", ".."])

isInt :: String -> Bool
isInt = all (`elem` "0123456789")

allSettings :: IO [Func]
allSettings = do
  cpus <- listFiles (\s -> isPrefixOf "cpu" s && isInt (drop 3 s)) "/sys/devices/system/cpu/"
  usbs <- listFiles (const True) "/sys/bus/usb/devices/"
  devices <- fmap concat $ listFiles (const True) "/sys/bus/" >>= mapM (listFiles (const True) . (</> "devices"))

  return $
    -- Sound
    [ File "1" "1" "/sys/module/snd_ac97_codec/parameters/power_save"
    , File "1" "1" "/sys/module/snd_hda_intel/parameters/power_save"
    , File "1" "1" "/sys/module/snd_hda_intel/parameters/power_save_controller"

    -- Kernel stuff
    , File "0"         "0"           "/proc/sys/kernel/nmi_watchdog"
    , File "powersave" "performance" "/sys/module/pcie_aspm/parameters/policy"

    -- CPUS
    , File "1" "1" "/sys/devices/system/cpu/sched_mc_power_savings"

    -- Disks
    , File "5"     "0"   "/proc/sys/vm/laptop_mode"
    , File "90"    "30"  "/proc/sys/vm/dirty_ratio"
    , File "1"     "10"  "/proc/sys/vm/dirty_background_ratio"
    , File "60000" "600" "/proc/sys/vm/dirty_expire_centisecs"
    , File "60000" "600" "/proc/sys/vm/dirty_writeback_centisecs"

    -- Modules
    , StopModule "bluetooth"
    , StopModule "btusb"
    , StopModule "hci_usb"
    , StopModule "nouveau"
    , StopModule "uvcvideo" -- webcam

    ] ++ map (\cpu -> File "ondemand" "ondemand" (cpu </> "cpufreq/scaling_governor")) cpus
      ++ map (\usb -> File "2" "2" (usb </> "power/autosuspend")) usbs
      ++ map (\device -> File "auto" "auto" (device </> "power/control")) devices

filterSetting :: Func -> IO Bool
filterSetting (File _ _ f) = doesFileExist f
filterSetting _            = return True

funcToCppCheck :: Func -> String
funcToCppCheck (File _ _ p)   = "check(\"" ++ p ++ "\");"
funcToCppCheck (StopModule m) = "is_loaded(\"" ++ m ++ "\");"

main :: IO ()
main = allSettings >>= filterM filterSetting >>= mapM_ (putStrLn . funcToCppCheck)
