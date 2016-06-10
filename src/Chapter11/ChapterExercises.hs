module Chapter11.ChapterExercises where
import           Data.Char     (toUpper)
import           Data.Function (on)
import           Data.List     (groupBy, elemIndices)

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf a@(x:a') (y:b) | x == y    = isSubsequenceOf a' b
                               | otherwise = isSubsequenceOf a b

capitalizeWord :: String -> String
capitalizeWord (h:t)
  | h == ' ' = h : capitalizeWord t
  | otherwise = toUpper h : t
capitalizeWord [] = []

capitalizeWords :: String -> [(String,String)]
capitalizeWords s =
  let capitalizeFirst w = (w, capitalizeWord w)
  in map capitalizeFirst (words s)

capitalizeParagraph :: String -> String
capitalizeParagraph xs = concatMap capitalizeWord $ groupBy ((==) `on` (=='.')) xs

type Presses = Int
type Digit = Int

data Phone = Phone [(Int, String)] deriving Show

keyPresses :: Phone -> Char -> (Digit, Presses)
keyPresses (Phone p) c =
  let (key, values) = head $ filter (\(_, v) -> elem c v) p
      presses = (+1) $ head $ elemIndices c values
  in (key, presses)

reverseTaps :: Phone -> Char -> [(Digit, Presses)]
reverseTaps p c
  | toUpper c == c = [keyPresses p '^', keyPresses p c]
  | otherwise = [keyPresses p (toUpper c)]

cellPhonesDead :: Phone -> String -> [(Digit, Presses)]
cellPhonesDead = undefined

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = undefined

mostPopularLetter :: String -> Char
mostPopularLetter = undefined

coolestLtr :: [String] -> Char
coolestLtr = undefined

coolestWord :: [String] -> String
coolestWord = undefined
