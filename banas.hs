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
