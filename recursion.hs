import Data.List
import System.IO

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- much simpler method
prodFact :: Int -> Int
prodFact n = product [1..n]

-- could do this with isOdd n = n `mod` 2 == 0, wanted to introduce guards
isOdd :: Int -> Bool
isOdd n
  | n `mod` 2 == 0 = False
  | otherwise = True


whatGrade :: Int -> String
whatGrade age
  | (age >= 6) && (age <= 14) = "Elementary"
  | (age >= 14) && (age <= 18) = "High School"
  | (age > 18) = "older then High School"
  | otherwise = "kiddy kid"

batAvgRating :: Double -> Double -> String
batAvgRating hits atBats
  | avg <= 0.200 = "Terrible"
  | avg <= 0.250 = "Average"
  | avg <= 0.280 = "Pretty good"
  | otherwise = "Superstar"
  where avg = hits / atBats

getListItems :: [Int] -> String
getListItems [] = "Empty"
getListItems (x:[]) = "Your list starts with " ++ show x
getListItems (x:y:[]) = "Your list contains " ++ show x ++ " and " ++ show y
getListItems (x:xs) = "First: " ++ show x ++ " and the rest: " ++ show xs

getFirstItem :: String -> String
getFirstItem [] = "Empty String"
getFirstItem all@(x:xs) = "The first letter in " ++ all ++ " is " ++ [x]
