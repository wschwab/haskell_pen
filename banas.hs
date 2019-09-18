-- exercising Haskell using Derek Banas's tutorial

import Data.List
import System.IO

primeNumbers = [3,5,7,11]

morePrime = primeNumbers ++ [13,17,19,23,29]

favNums = 2 : 7 : 21 : 66 :[]

morePrime2 = 2: morePrime

lenPrime = length morePrime2

thirdPrime = morePrime2 !! 2

lastPrime = last morePrime2

primeInit = init morePrime2

is7In = 7 `elem` morePrime2

letList = ['A','C'..'Z']

lotsaX = take 50 (repeat 2)

cycler = take 10 (cycle ['A', 'B', 'C'])

listTimes2 = [x * 2 | x <- [1..10]]

nine13 = [x | x <- [1..500], x `mod` 13 == 0, x `mod` 9 == 0]

multOfList = foldl (*) 2 [3,4,5]

pow3List = [3^n | n <- [1..10]]

multTable = [[x * y | y <- [1..12]] | x <- [1..12]]


addMe :: Int -> Int -> Int
addMe x y = x + y

-- will work with other kinds of numbers too
sumMe x y = x + y

addTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuples (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

whatAge :: Int -> String
whatAge 16 = "You can drive"
whatAge 18 = "You can vote"
whatAge 21 = "You can buy drinks"
whatAge x = "Nothing important"

times4 :: Int -> Int
times4 x = x * 4

listTimes4 = map times4 [1,2,3,4,5]

multBy4 :: [Int] -> [Int]
multBy4 [] = []
multBy4 (x:xs) = times4 x : multBy4 xs

areStringsEqual :: [Char] -> [Char] -> Bool
areStringsEqual [] [] = True
areStringsEqual (x:xs) (y:ys) = x == y && areStringsEqual xs ys
areStringsEqual _ _ = False

doMult :: (Int -> Int) -> Int
doMult func = func 3

num3Times4 = doMult times4

getAddFunc :: Int -> (Int -> Int)
getAddFunc x y = x + y

add3 = getAddFunc 3
fourPlus3 = add3 4

-- lambdas
dbl1To10 = map (\x -> x * 2) [1..10]

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
