import System.Environment (getArgs)

import Data.Char ( toLower )
import Data.List (foldl')

data Direction = Forward | Down | Up deriving (Show)

dirFromString :: String -> Direction
dirFromString s = case map toLower s of
    "forward" -> Forward
    "down" -> Down
    "up" -> Up
    _ -> error "no parse"


data Move = Move { direction :: Direction, distance :: Int } deriving (Show)

moveFromString :: String -> Move
moveFromString s = let
    ws = words s
    dir = dirFromString . head $ ws
    dist = read (head . tail $ ws) :: Int
    in Move dir dist

parseMoves :: String -> [Move]
parseMoves = map moveFromString . lines

finalPos :: [Move] -> (Int, Int)
finalPos = firstTwo . foldl' combine (0, 0, 0)
    where combine (h, v, a) m = case direction m of
                               Forward -> (distance m + h, v + distance m * a, a)
                               Down -> (h, v, a + distance m)
                               Up -> (h, v, a - distance m)
          firstTwo (x, y, _) = (x, y)

main :: IO ()
main = do
    fname <- head <$> getArgs
    payload <- readFile fname

    let 
        fp = (finalPos . parseMoves) payload
        prod = uncurry (*) fp in
        print fp >> print prod
    