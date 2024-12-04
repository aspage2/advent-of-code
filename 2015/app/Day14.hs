module Day14 where

import Parser
import Control.Monad (replicateM_)
import Data.Bifunctor (second)
import Data.List (foldl')
import Data.Char (isSpace)

import qualified Data.Map as M

data Pattern = Pattern { spd :: !Int, runDur :: !Int, restDur :: !Int } deriving Show

roundDistance :: Pattern -> Int
roundDistance p = spd p * runDur p

totalRound :: Pattern -> Int
totalRound p = runDur p + restDur p

flight :: Int -> Pattern -> Int
flight dur p = let (numRounds, rest) = dur `divMod` totalRound p
             in numRounds * roundDistance p + (spd p * min rest (runDur p))

word :: Parser String
word = while (not . isSpace) >>= (\s -> whiteSpace >> return s)

parse :: Parser (String, Pattern)
parse = do
    name <- word
    replicateM_ 2 word
    speed <- integer
    whiteSpace
    replicateM_ 2 word
    speedDur <- integer
    whiteSpace
    replicateM_ 6 word
    restDuration <- integer
    return (name, Pattern speed speedDur restDuration)
 
getPattern :: String -> (String, Pattern)
getPattern s = case runParser parse s of
    Left e -> error (show e)
    Right (r, _) -> r

maxBy :: Ord a => (b -> a) -> [b] -> b
maxBy _ [] = error "empty, bruh"
maxBy f (x:xs) = fst $ foldl' step (x, f x) xs
    where step (best, score) cur | f cur > score = (cur, f cur)
                                 | otherwise      = (best, score)

leaders :: Int -> [(String, Pattern)] -> [String]
leaders rnd players = let
    scores = map (second $ flight rnd) players
    maxScore = snd $ maxBy snd scores
    in map fst $ filter ((==maxScore).snd) scores

simulate :: Int -> [(String, Pattern)] -> M.Map String Int
simulate numRounds players = let leaderList = [1..numRounds] >>= \rnd -> leaders rnd players
    in foldl' (\m name -> M.insertWith (+) name 1 m) M.empty leaderList

dayMain :: String -> IO ()
dayMain fname = do
    f <- map getPattern . lines <$> readFile fname

    print $ length f

    let g = [("Dancer", Pattern 16 11 162), ("Comet", Pattern 14 10 127)]

    print $ simulate 1000 g
    print $ M.elems $ simulate 2503 f

