-- based pn tsoding's "HaskellRank" series

import Data.List
import System.IO

-- first exercise set
-- this will add any amount of numbers and output to screen
main = interact $ show . sum . map read . words

-- this one shaves off the first value as a technicality from how the problem was structured
main1 = interact $ show .sum . map read . tail . words

-- second set
-- given a number between 0-100, this needs to round to the nearest 5 if it's
-- less than 3 away (ends in a 3,4,8,9) as long as the number is also over 38
-- the first value is shaved off as a technicality in problem structure
-- solve2 names as such to differentiate by exercise number
round5 :: Int -> Int
round5 x
    | x >= 38 && (m5 - x) < 3 = m5
    | otherwise               = x
    where m5 = x + (5 - x `mod` 5)

solve2 :: [Int] -> [Int]
solve2 xs = map round5 xs

main2 = interact $ unlines . map show . solve2 . map read . tail .words

-- third set
-- this will look pretty random - check out HackerRank's 'Apples and Oranges'
-- problem if you're looking for context
solve3 :: [Int] -> [Int]
solve3 (s:t:a:b:m:_:rest) = [apples, oranges]
    where apples  = length $ filter (\x -> s <= x && x <= t) $ map (\x -> x + a) $ take m rest
          oranges = length $ filter (\x -> s <= x && x <= t) $ map (\x -> x + b) $ drop m rest

main3 = interact $ unlines . map show . solve3 . map read . words

-- fourth set
-- get middle letter (if odd) or letters (if even) of a char string
getMiddle :: String -> String
getMiddle s
    | odd n     = [s !! halfN]
    | otherwise = [s !! (halfN -1), s !! halfN]
    where halfN = length s

-- fifth set
-- from two lists return how many numbers fulfill 1) all numbers of first array
-- are factors, and 2) the number is a factor of all numbers in the second
-- more properly, the number is a product of the LCM of the first, and a divisor
-- of the GCD of the second
solve5 :: [Int] -> [Int] -> Int
solve5 as bs = length
                 $ filter (\x -> bsGCD `mod` x == 0)
                 $ takeWhile (<= bsGCD)
                 $ map (* asLCM) [1..]
    where asLCM = foldl1 lcm as
          bsGCD = foldl1 gcd bs

readIntList :: IO [Int]
readIntList = do line <- getLine
                 return $ map read $ words line

main5 = do [n, m] <- readIntList
           as     <- readIntList
           bs     <- readIntList
           putStrLn $ show $ solve5 as bs
