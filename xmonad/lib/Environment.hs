module Environment where

import System.Environment

getEnvDefault :: String -> String -> IO String
getEnvDefault env def = getEnv env `catch` (\_ -> return def)

