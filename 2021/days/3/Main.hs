import Data.List
import System.Environment
import Control.Applicative

------------ Helpers ------------------

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

-- Create an Integer from the boolean list, treating the list as bits.
-- toInt [True, True, False, False] => 10
toInt :: [Bool] -> Int
toInt bs = let
    pairs = enumerate (map conv . reverse $ bs)
    in sum . map (\(n, p) -> p * (2 ^ n)) $ pairs
    where conv True = 1
          conv False = 0

-- Re-stringify a 2d boolean list
toStr :: [[Bool]] -> String
toStr = intercalate "\n" . map (map (\b -> if b then '1' else '0'))

-- Parse a bitstring into an array of booleans (1=True, 0=False)
toBools :: String -> [Bool]
toBools = map (=='1')

------------ Part 1 ---------------

bitdiff :: [[Bool]] -> [Int]
bitdiff bs = foldr foldop zeros bs where
    zeros = map (const 0) (head bs)
    foldop = zipWith (\b x -> if b then x + 1 else x - 1)

gamma :: [[Bool]] -> Int
gamma = toInt . map (>0) . bitdiff

epsilon :: [[Bool]] -> Int
epsilon = toInt . map (<0) . bitdiff

powerConsumption :: [[Bool]] -> Int
powerConsumption = liftA2 (*) gamma epsilon


---------- Part 2 -------------

mcb :: Int -> [[Bool]] -> Int
mcb i = foldr (foldop . (!!i)) 0
    where foldop v acc = if v then acc + 1 else acc - 1

filterox :: Int -> [[Bool]] -> [[Bool]]
filterox i bs = case mcb i bs of 
    v | v >= 0 -> filter (!!i) bs
      | otherwise -> filter (not . (!!i)) bs

filterco2 :: Int -> [[Bool]] -> [[Bool]]
filterco2 i bs = case mcb i bs of
    v | v >= 0 -> filter (not . (!!i)) bs
      | otherwise -> filter (!!i) bs


filterStep :: (Int -> [[Bool]] -> [[Bool]]) -> [[Bool]] -> Int
filterStep f ls = _fs 0 ls where
    len = length . head $ ls
    _fs _ [] = undefined 
    _fs _ [line] = toInt line
    _fs i ls' = let newls = f (i `mod` len) ls' in _fs (i + 1) newls


ogr :: [[Bool]] -> Int
ogr = filterStep filterox

csr :: [[Bool]] -> Int
csr = filterStep filterco2

main :: IO ()
main = do
    payload <- getArgs >>= readFile . head
    let bs = map toBools . lines $ payload
    print $ liftA2 (*) ogr csr bs
