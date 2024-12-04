{-# LANGUAGE TupleSections #-}

module Day9 where

import qualified Data.Map as M

import Control.Monad (liftM2)
import Data.Either (fromRight)
import Data.List (foldl', nub, permutations)
import Data.Maybe (mapMaybe)
import Text.Read (readEither)

parseLine :: String -> Either String (String, String, Int)
parseLine s = case words s of 
    [a, _, b, _, c] -> (a, b,) <$> readEither c
    _else           -> Left "not the right number of components"

type Graph = M.Map (String, String) Int

pairSort :: (String, String) -> (String, String)
pairSort (a, b) | a < b     = (a, b)
                | otherwise = (b, a)

graphLookup :: Graph -> String -> String -> Maybe Int
graphLookup g a b = M.lookup (pairSort (a, b)) g

makeGraph :: [(String, String, Int)] -> M.Map (String, String) Int
makeGraph = foldl' f M.empty 
    where f g (a, b, dist) = M.insert (pairSort (a, b)) dist g

tripDistance :: [String] -> Graph -> Maybe Int
tripDistance p g = let
    edges =  zipWith (graphLookup g) p (drop 1 p) 
    in foldl' (liftM2 (+)) (Just 0) edges

uniqueNodes :: Graph -> [String]
uniqueNodes g = nub $ M.keys g >>= \(a, b) -> [a, b]

dayMain :: String -> IO ()
dayMain fname = do
    ls <- lines <$> readFile fname
    
    let g = makeGraph $ map (fromRight ("", "", 1) . parseLine) ls
    -- This really is only feasible because there are only 8
    -- nodes in the test input, which still ends up as 40K
    -- routes to test.
    let perms = permutations (uniqueNodes g)

    print $ minimum $ mapMaybe (`tripDistance` g) perms
    print $ maximum $ mapMaybe (`tripDistance` g) perms

