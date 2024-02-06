
import System.Environment
import Common.Helper
import Data.List

type FuelModel = Int -> Int -> Int


{- Day 7: Crabs

The goal is to choose the horizontal position which minimizes the total amount
of fuel needed to align the submarine crabs.

The fuel model in both parts can be modeled with simple match expressions:

For the simple case, fuel drops by 1 unit for every unit distance travelled, so
the amount of fuel is |position - target|.

For the second case, every step requires 1 more unit of fuel than the last, so the
amount of fuel for d = |position - target| => 1 + 2 + ... + d = d * (d + 1) / 2.

The brute-force minimization approach is O(n * m), where n is the number of crabs 
and m is the size of the problem-bounds, i.e. the distance between the farthest-right 
crab and the farthest-left crab.

-}

simpleFuelModel :: FuelModel
simpleFuelModel to from = abs (to - from)

realFuelModel :: FuelModel
realFuelModel to from = let n = abs (to - from) in n * (n + 1) `div` 2

fuelCost :: FuelModel -> [Int] -> Int -> Int
fuelCost fm crabs i = sum $ map (fm i) crabs

minFuelCost :: FuelModel -> [Int] -> Int
minFuelCost fm crabs = let
    mn = minimum crabs
    mx = maximum crabs
    in minimum $ map (fuelCost fm crabs) [mn..mx]

problemParse :: String -> [Int]
problemParse = map read . splitOn ','

main = do
    d <- problemParse <$> (getArgs >>= readFile . head)

    let mn = minimum d
    let mx = maximum d
    let f = intercalate "," $ map (show . fuelCost simpleFuelModel d) [mn..mx]
    putStrLn f