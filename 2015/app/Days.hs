module Days where

import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14

type Day = String -> IO ()

days :: [(Int, Day)] 
days = [
    (1, Day1.dayMain)
  , (2, Day2.dayMain)
  , (3, Day3.dayMain)
  , (4, Day4.dayMain)
  , (5, Day5.dayMain)
  , (6, Day6.dayMain)
  , (7, Day7.dayMain)
  , (8, Day8.dayMain)
  , (9, Day9.dayMain)
  , (10, Day10.dayMain)
  , (11, Day11.dayMain)
  , (12, Day12.dayMain)
  , (13, Day13.dayMain)
  , (14, Day14.dayMain)
 ]

selectDay :: Int -> Either String Day
selectDay d = foldr finder ifErr days
    where ifErr = Left $ "Day #"++show d++" does not exist"
          finder (n, day') acc = if n == d 
                                then Right day'
                                else acc

