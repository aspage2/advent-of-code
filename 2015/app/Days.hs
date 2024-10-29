module Days where

import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5

type Day = String -> IO ()

days :: [(Int, Day)] 
days = [
    (1, Day1.dayMain)
  , (2, Day2.dayMain)
  , (3, Day3.dayMain)
  , (4, Day4.dayMain)
  , (5, Day5.dayMain)
 ]

selectDay :: Int -> Either String Day
selectDay d = foldr finder ifErr days
    where ifErr = Left $ "Day #"++show d++" does not exist"
          finder (n, day') acc = if n == d 
                                then Right day'
                                else acc

