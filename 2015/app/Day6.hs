module Day6 where

import Parser (runParser, Parser, str, char, whiteSpace, integer)

import Control.Monad (liftM2)
import Control.Applicative ((<|>))
import Control.Monad.ST (ST, runST)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM


type Op = (Int -> Int)
data Command = Command !Op !(Int, Int) !(Int, Int)

parse :: Op -> Op -> Op -> Parser Command
parse on off togg = do
    op <- (str "turn on" >> return on)
      <|> (str "turn off" >> return off)
      <|> (str "toggle" >> return togg)

    _ <- whiteSpace

    fr <- liftM2 (,) integer (char ',' >> integer)

    _ <- whiteSpace >> str "through" >> whiteSpace

    to <- liftM2 (,) integer (char ',' >> integer)

    return $ Command op fr to

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

part1Command :: Parser Command
part1Command = parse (const 1) (const 0) (\x -> if x > 0 then 0 else 1)

part2Command :: Parser Command
part2Command = parse (+1) (max 0 . (-1+)) (+2)

mapEither :: (a -> Either b c) -> [a] -> [c]
mapEither _ [] = []
mapEither f (x:xs) = case f x of
    Right c -> c : mapEither f xs
    Left _ -> mapEither f xs

dayMain :: String -> IO ()
dayMain fname = do
    ls <- lines <$> readFile fname
    let p1commands = mapEither (runParser part1Command) ls
    let p2commands = mapEither (runParser part2Command) ls

    print $ sum $ simulate $ map fst p1commands
    print $ sum $ simulate $ map fst p2commands
