module Main where

import Control.Monad
import Data.List
import System.Directory
import System.Environment
import System.FilePath

-- TODO: More power modes
-- TODO: Auto detect default values

type Full = String
type Save = String

data Func = File Save Full FilePath
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
  scsi_hosts <- listFiles (const True) "/sys/class/scsi_host/"

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
    , File "1" "1" "/sys/devices/system/cpu/sched_smt_power_savings"

    -- Disks
    , File "5"    "0"    "/proc/sys/vm/laptop_mode"
    , File "90"   "20"   "/proc/sys/vm/dirty_ratio"
    , File "1"    "10"   "/proc/sys/vm/dirty_background_ratio"
    , File "1500" "3000" "/proc/sys/vm/dirty_expire_centisecs"
    , File "1500" "500"  "/proc/sys/vm/dirty_writeback_centisecs"

    -- Modules
    --, StopModule "bluetooth"
    --, StopModule "btusb"
    --, StopModule "hci_usb"
    --, StopModule "nouveau"
    --, StopModule "uvcvideo" -- webcam

    ] ++ map (\cpu -> File "ondemand" "ondemand" (cpu </> "cpufreq/scaling_governor")) cpus
      ++ map (\usb -> File "2" "2" (usb </> "power/autosuspend")) usbs
      ++ map (\device -> File "auto" "auto" (device </> "power/control")) devices
      ++ map (\host -> File "min_power" "max_performance" (host </> "link_power_management_policy")) scsi_hosts

filterSetting :: Func -> IO Bool
filterSetting (File _ _ f) = doesFileExist f
filterSetting _            = return True

funcToCppCheck :: Func -> String
funcToCppCheck (File _ _ p)   = showString "check(" $ shows p ");"
funcToCppCheck (StopModule m) = showString "is_loaded(" $ shows m ");"

funcToCppSave :: Func -> String
funcToCppSave (File v _ p)   = showString "opt(" $ shows p $ showString ", " $ shows v ");"
funcToCppSave (StopModule m) = showString "unload(" $ shows m ");"

funcToCppFull :: Func -> String
funcToCppFull (File _ v p)   = showString "opt(" $ shows p $ showString ", " $ shows v ");"
funcToCppFull (StopModule m) = showString "load(" $ shows m ");"

mainArg :: [String] -> IO ()
mainArg arg = case arg of
  ["check"] -> go funcToCppCheck
  ["save"]  -> go funcToCppSave
  ["full"]  -> go funcToCppFull
  _         -> putStrLn "specific one of: check, save, full"
  where
    go f = allSettings >>= filterM filterSetting >>= mapM_ (putStrLn . f)

main :: IO ()
main = getArgs >>= mainArg
