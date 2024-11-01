module Day6 where

import Control.Monad.State.Lazy (State, state, modify, evalState)
import Data.Char (isDigit)
import Data.List (isPrefixOf)

import Control.Monad.ST (ST, runST)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

type Op = (Int -> Int)
data Command = Command !Op !(Int, Int) !(Int, Int)

takeOp :: State String Op
takeOp = state f where
     f s | "turn on " `isPrefixOf` s = (const 1, drop 8 s)
         | "turn off " `isPrefixOf` s = (const 0, drop 9 s)
         | "toggle " `isPrefixOf` s = (\x -> if x > 0 then 0 else 1, drop 7 s)
         | otherwise = error "not an op"

takeOp2 :: State String Op
takeOp2 = state f where
     f s | "turn on " `isPrefixOf` s = ((+1), drop 8 s)
         | "turn off " `isPrefixOf` s = (\x -> max (x - 1) 0 , drop 9 s)
         | "toggle " `isPrefixOf` s = ((+2), drop 7 s)
         | otherwise = error "not an op"

splitWhile :: (a -> Bool) -> [a] -> ([a], [a])
splitWhile _ [] = ([], [])
splitWhile p full@(x:xs) | p x = let (a, rest) = splitWhile p xs in (x:a, rest)
                         | otherwise = ([], full)

takeNum :: State String Int
takeNum = read <$> state (splitWhile isDigit)

takeCommand :: State String Op -> State String Command
takeCommand to = do
    op <- to

    a <- takeNum
    modify (drop 1)
    b <- takeNum

    modify (drop 9) -- ' through '
    
    c <- takeNum
    modify (drop 1)
    d <- takeNum

    return $ Command op (a, b) (c, d)


newGrid :: ST s (VM.MVector s Int)
newGrid = VM.replicate (1000*1000) 0
    
gridIdx :: (Int, Int) -> Int
gridIdx (x, y) = 1000*x + y

gridCommand :: Command -> VM.MVector s Int -> ST s ()
gridCommand (Command op (r, c) (r', c')) g = mapM_ (VM.modify g op) idxs 
    where idxs = [gridIdx (r'', c'') | r'' <- [r..r'], c'' <- [c..c']]

simulate :: [Command] -> V.Vector Int
simulate commands = runST $ do
    g <- newGrid
    mapM_ (`gridCommand` g) commands
    V.unsafeFreeze g

dayMain :: String -> IO ()
dayMain fname = do
    ls <- lines <$> readFile fname
    let p1commands = map (evalState $ takeCommand takeOp) ls
    let p2commands = map (evalState $ takeCommand takeOp2) ls

    print $ sum $ simulate p1commands
    print $ sum $ simulate p2commands
