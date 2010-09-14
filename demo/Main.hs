module Main where

import Control.Concurrent

import Happstack.Server
import Happstack.State
import Happstack.Auth

import Route

appConf :: Conf
appConf = nullConf { port = 8080 }

type Control = MVar TxControl

withAuthState :: (Control -> IO ()) -> IO ()
withAuthState io = do
    control <- startSystemState authProxy
    putStrLn "State started."
    tid <- forkIO $ io control
    waitForTermination
    putStrLn "Shutting down..."
    killThread tid
    shutdownSystem control

main :: IO ()
main = withAuthState $ \_ -> do
    simpleHTTP appConf appRoute
