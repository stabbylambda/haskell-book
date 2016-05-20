module Chapter10.FoldingListsSpec where

import Chapter10.FoldingLists

import Test.Hspec
import Data.Time

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  do describe "theDatabase" $
       do it "filterDbDate gets the UTCTimes out of the DbDates" $
            filterDbDate theDatabase `shouldBe`
            [UTCTime (fromGregorian 1911 5 1)
                     (secondsToDiffTime 34123)
            ,UTCTime (fromGregorian 1921 5 1)
                     (secondsToDiffTime 34123)]
          it "gets the Integers out of the DbNumbers" $
            filterDbNumber theDatabase `shouldBe` [9001,2]
          it "gets the most recent of the DbDates" $
            mostRecent theDatabase `shouldBe`
            UTCTime (fromGregorian 1921 5 1)
                    (secondsToDiffTime 34123)
          it "mostRecent gets the most recent of the DbDates" $
            mostRecent theDatabase `shouldBe`
            UTCTime (fromGregorian 1921 5 1)
                    (secondsToDiffTime 34123)
          it "sumDb should sum the numbers" $ sumDb theDatabase `shouldBe` 9003
          it "avgDb should average the numbers" $
            avgDb theDatabase `shouldBe` 4501.5
     describe "Scan exercises" $
       do it "fibs20 returns only the first 20 fibonacci numbers" $ pending
          it "fibslt100 returns only fibonacci numbers less than 100" $ pending
          it "factorial with scan" $ pending
