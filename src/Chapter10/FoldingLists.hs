module Chapter10.FoldingLists where

import Data.Time

data DatabaseItem
  = DbString String
  | DbNumber Integer
  | DbDate UTCTime
  deriving (Eq,Ord,Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [DbDate (UTCTime (fromGregorian 1911 5 1)
                   (secondsToDiffTime 34123))
  ,DbNumber 9001
  ,DbNumber 2
  ,DbString "Hello, world!"
  ,DbDate (UTCTime (fromGregorian 1921 5 1)
                   (secondsToDiffTime 34123))]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr consDates []
  where consDates a b =
          case a of
            (DbDate date) -> date : b
            _ -> b

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr consIntegers []
  where consIntegers a b =
          case a of
            (DbNumber num) -> num : b
            _ -> b

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb = mean . filterDbNumber
  where mean xs = fromIntegral (sum xs) / fromIntegral (length xs)

fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs

fibsN :: Int -> Integer
fibsN x = fibs !! x
