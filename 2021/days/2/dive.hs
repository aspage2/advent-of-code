import System.Environment (getArgs)

import Data.Char ( toLower )
import Data.List (foldl')
import Control.Applicative
import Common.Parse
import Control.Monad.Trans.State

data Direction = Forward | Down | Up deriving (Show)

dirFromString :: String -> Direction
dirFromString s = case map toLower s of
    "forward" -> Forward
    "down" -> Down
    "up" -> Up
    _ -> error "no parse"


data Move = Move { direction :: Direction, distance :: Int } deriving (Show)

moveFromString :: String -> Move
moveFromString = evalState $ liftA2 Move
    (dirFromString <$> delim ' ')
    (read <$> get)

parseMoves :: String -> [Move]
parseMoves = map moveFromString . lines

finalPos :: [Move] -> (Int, Int)
finalPos = foldl' combine (0, 0)
    where combine (h, v) m = case direction m of
                               Forward -> (distance m + h, v)
                               Down -> (h, v + distance m)
                               Up -> (h, v - distance m)

productOfFinalMoves :: [Move] -> Int
productOfFinalMoves = uncurry (*) . finalPos

main :: IO ()
main = do
    fname <- head <$> getArgs
    payload <- readFile fname
    
    print $ (productOfFinalMoves . parseMoves) payload