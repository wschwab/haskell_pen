import Data.List
import System.IO

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
