import qualified Data.Array as A
import System.Environment
import Data.List
import Common.Helper

{- Day 13 - Folding (of the paper variety)

When we fold along a line, we're creating a new collection of points
inside the "lower" portion of the line (e.g. above a horizontal line,
left of a vertical line).

 * All points in the "lower" region stay put
 * All points in the "upper" region are reflected about the fold line.

-}

data Line = Horizontal Int | Vertical Int deriving (Show)

-------- Parse --------
parsePoint :: String -> (Int, Int)
parsePoint s = let
    (f, s') = break (==',') s 
    in (read f, read (tail s'))

parseLine :: String -> Line
parseLine s = let eq = last . words $ s
                  (f, s') = break (=='=') eq
              in case f of
                  "y" -> Horizontal (read (tail s'))
                  "x" -> Vertical (read (tail s'))
                  _ -> undefined 

parse :: String -> ([(Int, Int)], [Line])
parse s = let 
    ls = lines s 
    (pts, eqs) = break (=="") ls
    in (map parsePoint pts, map parseLine (tail eqs))

--------- Fold ---------

reflect :: Line -> (Int, Int) -> (Int, Int)
reflect (Horizontal i) (x, y) = (x, y - 2 * (y - i))
reflect (Vertical i) (x, y) = (x - 2 * (x - i), y)

lt :: Line -> (Int, Int) -> Bool
lt (Horizontal i) (_, y) = y < i
lt (Vertical i) (x, _) = x < i

paperFold :: Line -> [(Int, Int)] -> [(Int, Int)]
paperFold l = dedupe . sort . map convert
    where
        convert pt = if lt l pt
                        then pt
                        else reflect l pt

{-
    Because our fold operation is non-associative (order matters),
    we have to use foldl here to make sure l1 is applied first.

    foldl -> f (... (f (f pts l1) l2) ...) lN => l1 is used first
    foldr -> f l1 (f l2 (... (f lN pts) ...)) => l1 is used last
-}
paperFolds :: [Line] -> [(Int, Int)] -> [(Int, Int)]
paperFolds ls pts = foldl (flip paperFold) pts ls

--------- Display ---------

toString :: [(Int, Int)] -> String
toString pts = let
    xmin = (minimum . map fst) pts
    xmax = (maximum . map fst) pts
    ymin = (minimum . map snd) pts
    ymax = (maximum . map snd) pts

    arr = A.array ((xmin, ymin), (xmax, ymax)) [((i, j), False) | i <- [xmin..xmax], j <- [ymin..ymax]]

    arr' = arr A.// map (\pt -> (pt, True)) pts
    row = (\a y -> map (\x -> if a A.! (x, y) then '#' else '.') [xmin..xmax])
    in unlines (map (row arr') [ymin..ymax])

main = do
    contents <- getArgs >>= readFile . head

    let (pts, ls) = parse contents
    let pts' = paperFolds ls pts

    putStrLn $ toString pts'
    