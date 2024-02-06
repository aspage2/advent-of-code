import Data.Maybe
import System.Environment
import Data.List
import Control.Monad
import Control.Monad.Trans.State
import Common.Parse

{- Day 4: Bingo

Using Maybe to account for boards which can't be solved with the given call sequence.

Haskell-y things
 * Using Maybe's monadic properties to add short-circuiting to strings of operations
   e.g. if the first call returns "Nothing", skip the remaining calls and return Nothing.
 * Using the "Ord" typeclass to roll min/max functions which work with Maybe types
-}

type Board = [Int]
type CallSequence = [Int]

column :: Int -> [a] -> [a]
column c [] = []
column c bs = let
    bs' = drop c bs
    val = head bs'
    bs'' = drop (5 - c) bs'
    in val : column c bs''

row :: Int -> [a] -> [a]
row r bs = let
    bs' = drop (5 * r) bs
    in take 5 bs'

_cf :: Ord a => (a -> a -> Bool) -> [Maybe a] -> Maybe a
_cf cmp = foldr step Nothing where
    step Nothing acc = acc
    step val Nothing = val
    step (Just v) (Just a) = if cmp v a then Just v else Just a

minMaybe = _cf (<)
maxMaybe = _cf (>)

-- The latest bingo call which will satisfy this span.
latestCall :: CallSequence -> [Int] -> Maybe Int
latestCall draws span = maxMaybe (map drawNum span) where
    drawNum spc = elemIndex spc draws

-- The call number which will make this board win.
winningCall :: CallSequence -> Board -> Maybe Int
winningCall calls bs = let
    spans = liftM2 (\x y -> x y bs) [row, column] [0..4]
    spanScores = map (latestCall calls) spans
    in minMaybe spanScores

-- The score of a board
boardScore :: Int -> CallSequence -> Board -> Int
boardScore call calls b = let
    cn = calls !! call
    in sum (filter (\c -> fromJust (elemIndex c calls) > call) b) * cn

-- Maybe is a Monadic type, where maybe's bind (>>=) operation
-- short-circuits if the left value is Nothing. This means we can
-- safely perform a sequence of calls like so using `do` notation.
boardInfo :: CallSequence -> Board -> Maybe (Int, Int)
boardInfo cs b = do
    wc <- winningCall cs b
    return (wc, wc * boardScore wc cs b)

--------------- Parsing --------------------------
valSplit :: Eq a => a -> [a] -> [[a]]
valSplit c = _vs where
    _vs s = case dropWhile (==c) s of
        [] -> []
        s' -> w : valSplit c s'' where (w, s'') = break (==c) s'

parseBoard :: State String Board
parseBoard = concat <$> replicateM 5 parseBoardRow
    where parseBoardRow = map read . valSplit ' ' <$> delim '\n'

parseManyBoards :: State String [Board]
parseManyBoards = do
    s <- get
    if null s
        then return []
        else liftM2 (:) (trimLeft '\n' >> parseBoard) parseManyBoards

parseFile :: State String ([Int], [Board])
parseFile = liftM2 (,)
    (map read . valSplit ',' <$> delim '\n')
    parseManyBoards



o x y | snd x > snd y = GT
      | snd x < snd y = LT
      | otherwise = EQ

main :: IO ()
main = do
    payload <- getArgs >>= readFile . head
    let (ins, bs) = evalState parseFile payload

    let info = mapMaybe (boardInfo ins) bs

    print $ minimumBy o info
    print $ maximumBy o info


