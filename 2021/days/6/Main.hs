import System.Environment

import Data.Maybe
import qualified Data.Map as Map
import Common.Helper


{- Day 6: Lanternfish

The timer-state of each fish is simple and independant of the other lanternfish, so
we can track the population in "buckets" instead of by individual fish. Namely, the
state for the colony is divided into 9 buckets [0, 8], where bucket i is the number
of fish in the colony with timer value i.

To transition the ColonyState to the next day, each lanternfish's internal timer goes
down by one, effectively shifting our bucket counts down by one bucket. e.g. for
buckets [0, 8):

    B_new[i] = B_old[i+1]

To account for the lanternfish which were in bucket 0, we add that bucket count to
buckets #6 and #8, respectively representing the bucket #0 lanternfish whose timer reset to 6,
and the newborn lanternfish whose timer starts at 8.
-}

type ColonyState = Map.Map Int Int


newColonyState :: [Int] -> ColonyState
newColonyState = let
    st = Map.fromList (zip [0..8] (repeat 0))
    in foldr step st
    where step age = Map.insertWith (+) age 1

nextState :: ColonyState -> ColonyState
nextState m = let
    nz = fromJust $ Map.lookup 0 m
    nm = foldr step Map.empty [0..8]
    in Map.insertWith (+) 6 nz nm
    where step i accM = Map.insert i (fromJust $ Map.lookup ((i + 1) `mod` 9) m) accM

atDay :: Int -> ColonyState -> ColonyState
atDay n = last . take (n + 1) . iterate nextState

numFish :: ColonyState -> Int
numFish = sum . Map.elems

main :: IO ()
main = do
    start <- map read . splitOn ',' <$> (getArgs >>= readFile . head)
    let st = newColonyState start
    print $ numFish . atDay 256 $ st
    