{-# LANGUAGE OverloadedStrings #-}

import qualified WebServer as W
import qualified EventGenerator as E
import qualified Database.Redis as R
import qualified Pacemaker as P
import Control.Concurrent
import qualified KeyConfig as K
import Data.IORef

main :: IO ()
main = do
      api_keys <- K.loadConf
      runDefaults api_keys                    

runDefaults :: IORef K.APIKeyMap -> IO ()
runDefaults api_keys = do   
  _ <- forkIO $ E.main (P.PaceRedis conn) api_keys
  W.main (P.PaceRedis conn) api_keys
  
  
  
conn :: R.ConnectInfo  
conn = R.defaultConnectInfo {
      R.connectHost = "redis"
  }  
