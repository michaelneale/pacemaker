{-# LANGUAGE OverloadedStrings #-}

module MetricsSpec (main, spec) where

import Test.Hspec
import Metrics
import Data.Aeson
import Test.QuickCheck
import qualified Data.Text as T

main :: IO ()
main = hspec spec


toLR content =
  case eitherDecode content of
    Right c -> c
    Left _ -> error $ "Doh"

fromStr :: LearnResponse
fromStr  = toLR "{\"anomalous\": true, \"likelihood\": 1.0, \"predicted\": 1.97673892962127, \"anomalyScore\": 1.0, \"current_value\": 1.9 }"


calc :: LearnResponse -> T.Text
calc lr = calculatedState $ Just lr

spec :: Spec
spec = do
  describe "Calculate state" $ do
    it "Should be anom as json says so" $ do
      (calc fromStr) `shouldBe` "CRITICAL" 
    it "Should be unknown as no learning response" $ do
      (calculatedState Nothing) `shouldBe` "UNKNOWN" 
    it "Should be OK as not anom" $ property $
      \(a, b, c, d) -> (calc (LearnResponse False a b c d)) == "OK" 
    it "Should be CRITICAL as anom" $ property $
      \(a, b, c, d) -> (calc (LearnResponse True a b c d)) == "CRITICAL" 
      
      
      
      
    
