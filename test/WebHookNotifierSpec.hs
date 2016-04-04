{-# LANGUAGE OverloadedStrings #-}

module WebHookNotifierSpec (main, spec) where

import Test.Hspec
import WebHookNotifier
import qualified Data.Text as T
import qualified HistoricalData as HS

main :: IO ()
main = hspec spec



spec :: Spec
spec = do
  describe "Alert notifications" $ do
    it "Should pick this up as bad" $ do
      isAlertable [h 500 "CRITICAL", h 400 "CRITICAL", h 300 "CRITICAL"] 401 `shouldBe` (True, 300)
    it "Should not pick this up" $ do
      isAlertable [h 420 "CRITICAL", h 410 "OK", h 400 "CRITICAL"] 30 `shouldBe` (False, 420)
    it "Should not pick this up" $ do
      isAlertable [] 41 `shouldBe` (False, 0)
    it "Should be long enough in failures" $ do
      isAlertable [h 450 "CRITICAL", h 400 "CRITICAL", h 350 "CRITICAL", h 300 "OK", h 250 "OK"] 351 `shouldBe` (True, 350)
    it "Should be long enough in failures" $ do
      isAlertable [h 32 "OK"] 351 `shouldBe` (False, 0)



h :: Integer -> T.Text -> HS.HistoryMetric
h tstamp state = HS.HistoryMetric {
  HS.current = Just 42,
  HS.predicted = Just 32,
  HS.timestamp = tstamp,
  HS.state = state
}  
