module Days where

import qualified Day1

type Day = String -> IO ()

days :: [(Int, Day)] 
days = [
    (1, Day1.dayMain)
 ]

selectDay :: Int -> Either String Day
selectDay d = foldr finder ifErr days
    where ifErr = Left $ "Day #"++show d++" does not exist"
          finder (n, day') acc = if n == d 
                                then Right day'
                                else acc

