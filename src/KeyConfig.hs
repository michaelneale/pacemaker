{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-} -- for the HashMap ToJSON instances

module KeyConfig where

import Data.Aeson
import Data.Text
import qualified Data.ByteString.Lazy as B
import GHC.Generics
import qualified Data.HashMap.Strict as HM
--import qualified Data.HashSet as HS
import qualified Data.Set as Set

import Data.IORef
import Control.Concurrent
import Control.Monad
import System.Environment
import Data.Maybe
import Network.HTTP.Conduit
import qualified Control.Exception    as E
import qualified Data.ByteString.Char8 as W

data APIKeyMap = 
    APIKeyMap {
      api_keys :: HM.HashMap Key Entity,
      webhook_config :: Maybe (HM.HashMap Key WebhookConfig)
    } deriving (Eq, Show, Generic)

data WebhookConfig = 
  WebhookConfig {
    alert_endpoint :: Text,
    seconds_failing :: Maybe Integer
  } deriving (Eq, Show, Generic)


instance FromJSON WebhookConfig

instance FromJSON APIKeyMap
  

parseAPIKeys :: B.ByteString -> APIKeyMap
parseAPIKeys content =
  case eitherDecode content of
    Right c -> c
    Left err -> error $ "Unable to decode api_keys.json " ++ (show err)



lookupEntity :: Text -> APIKeyMap -> Maybe Entity
lookupEntity apikey mp = HM.lookup apikey (api_keys mp)

-- hook key will be the entity/org
lookupHook :: Text -> APIKeyMap -> Maybe WebhookConfig
lookupHook hookKey mp = 
  HM.lookup hookKey hooks
  where
    hooks = fromMaybe HM.empty (webhook_config mp)
    

entities :: APIKeyMap -> [Entity]
entities mp = ordNub $ HM.elems $ api_keys mp--HS.toList $ HS.fromList $ HM.elems $ api_keys mp


data KeyCheck = KeyOK Text | InvalidKey
    deriving (Show, Eq)

checkKey :: Text -> APIKeyMap -> KeyCheck
checkKey apikey mp = maybe InvalidKey KeyOK $ lookupEntity apikey mp



type Entity = Text
type Key = Text



-- Dyanmically refresh the config - magic.
loadConf :: IO (IORef APIKeyMap)
loadConf = do
  newRef <- newIORef $ APIKeyMap HM.empty Nothing
  _ <- forkIO $ syncConfig newRef
  return newRef


fetchC :: String -> String -> IO (Maybe APIKeyMap)
fetchC url auth = withManager $ \manager -> do
                    res <- httpLbs r manager -- httpLbs for simpler fetching of body content                                                                                res <- httpLbs reqHead manager -- httpLbs for simpler fetching of body content                                        
                    return $ decode (responseBody res)
                where
                  r = applyBasicAuth (W.pack auth) "" (confUrl url) {
                        method = "GET",
                        checkStatus = \_ _  _ -> Nothing  -- stops it barfing on non 200OK FFS.                      
                       }

ld :: IO (Maybe APIKeyMap)
ld = do
  url <- env "CONSOLE_URL" "http://localhost:6789/api_keys.json"
  auth <- env "CONSOLE_AUTH" "rad"
  result <- E.try (fetchC url auth) :: IO (Either HttpException (Maybe APIKeyMap))
  case result of 
    Left e -> 
      do 
        print e
        return Nothing
    Right v -> return v


syncConfig :: IORef APIKeyMap -> IO ()
syncConfig checksRef = do    
    putStrLn $ "Monitoring rad app for api_key changes now"
    forever $ do
       putStrLn $ "-> starting check for changes"       
       loadedKeys <- ld
       case loadedKeys of
          Nothing -> putStrLn "WARNING: Unable to decode api_keys from rad alert app"
          Just newChecks -> maybeUpdateChecks checksRef newChecks
       threadDelay 60000000
       return ()


maybeUpdateChecks :: IORef APIKeyMap -> APIKeyMap -> IO ()
maybeUpdateChecks existing new = do
  ex <- readIORef existing
  if ex == new 
    then 
      putStrLn "-> No changes needed at this time... "        
    else 
      putStrLn "NOTE: changes are being applied" >>
      modifyIORef existing (\_ -> new)
      

confUrl :: String -> Request
confUrl url = 
  case parseUrl url of
        Nothing -> error "Unable to load the url for looking up api_keys"
        Just req -> req


env :: String -> String -> IO String
env keyName defaultVal = do
  val <- lookupEnv keyName
  return $ fromMaybe defaultVal val
  
-- thanks to: https://github.com/nh2/haskell-ordnub - although n is small, could use normal nub, or HS.toList . HS.fromList - who really cares.
-- Coffee cares. That's who.   
ordNub :: (Ord a) => [a] -> [a]
ordNub l = go Set.empty l
  where
    go _ [] = []
    go s (x:xs) = if x `Set.member` s then go s xs
                                      else x : go (Set.insert x s) xs
