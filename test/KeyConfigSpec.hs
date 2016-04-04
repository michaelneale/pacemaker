{-# LANGUAGE OverloadedStrings #-}

module KeyConfigSpec (main, spec) where

import Test.Hspec
import qualified KeyConfig as C
-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec


sampleKeys :: C.APIKeyMap
sampleKeys = C.parseAPIKeys "{\"api_keys\": { \"12\" : \"myentity\" }}"

sampleWithHook :: C.APIKeyMap
sampleWithHook = C.parseAPIKeys "{\"api_keys\": { \"12\" : \"myentity\" }, \"webhook_config\": { \"yeah:boo\":  {\"alert_endpoint\" : \"foo.com\" } } }"


sampleKeysDupe :: C.APIKeyMap
sampleKeysDupe = C.parseAPIKeys "{\"api_keys\": { \"12\" : \"myentity\", \"13\" : \"myentity\" }}"

spec :: Spec
spec = do
  describe "Check we can load api key map" $ do
    it "should parse json" $ do
      (C.lookupEntity "12" sampleKeys) `shouldBe` (Just "myentity")
    it "should validate apikey" $ do
      (C.checkKey "12" sampleKeys) `shouldBe` (C.KeyOK "myentity")
    it "should validate apikey missing" $ do
      (C.checkKey "123" sampleKeys) `shouldBe` C.InvalidKey
    it "should not have duplicate entities" $ do
      (length (C.entities sampleKeysDupe)) `shouldBe` 1
    it "should have webhook config" $ do
      (C.lookupHook "yeah:boo" sampleWithHook) `shouldBe` (Just $ C.WebhookConfig {C.alert_endpoint = "foo.com", C.seconds_failing = Nothing} )
    it "should not have webhook config" $ do
      (C.lookupHook "yeah:boo2" sampleWithHook) `shouldBe` Nothing
    it "should not have webhook config as none exist" $ do
      (C.lookupHook "yeah:boo" sampleKeys) `shouldBe` Nothing
        
  
          
          
