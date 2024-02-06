{-# LANGUAGE TupleSections #-}
import Data.Bifunctor
import Debug.Trace
import Control.Monad.Trans.State
import qualified Data.Map as M
import Control.Monad
import Common.Parse
import qualified Data.Set as S
import System.Environment
import Data.Maybe

{- Day 19 - Beacons

To find the 24 rotations, I locked the configuration of the 3 axis
so that they obey the right-hand rule:

https://en.wikipedia.org/wiki/File:Right_hand_rule_cross_product.svg

e.g. no matter how they are facing relative to your frame-of-reference,
you can orient your right hand in the pictured fashion so that:
 * your pointer finger lines up with the X axis.
 * your middle finger lines up with the Y axis
 * your thumb lines up with the Z axis.

Then, treating each of the 6 axes/directions as "real +z" one at a time,
I write out the "real +x", "real +y" and "real +z", locking each of the
four remaining axis/directions as "+x".

I used the following algorithm to align two scanner sets:

Align s0, s1:
    s0, s1 are sets of points
    Treat s0 as (0, 0, 0) facing the "correct" direction
    for each of the 24 roations R:
        s1' = rotate s1 using R
        for points p0, p1 in (s0 x s1'):
            vec = p1 - p0
            s1'' = translate s1' by vec
            if | s1'' `union` s0 | >= 12
                return (R, vec)
    nil - not enough points lined up in any case.

To align all scanners, maintain a list of all fixed scanners,
starting with just [s0]. Go through the unaligned scanner until
it can be aligned with one of the scanners in the fixed list. Add
the aligned scanner to the fixed list and repeat until the not-fixed
list is empty or one of the unfixed scanners can't be aligned.

Runtime as of commit is ~3:30 for my puzzle input.
-}

-- A space is a collection of points scanned by a scanner
type Space = S.Set Pt

-- A vector represents a translation in 3 dimensions.
type Vec = (Int, Int, Int)

-- A point is a 3D coordinate on some reference axis.
type Pt = (Int, Int, Int)

-- Parse

parseCoord :: String -> Pt
parseCoord = let
    d = read <$> delim ','
    rest = read <$> get
    in evalState (liftM3 (,,) d d rest)


parseScanner :: State [String] Space
parseScanner = S.fromList <$> (takeN 1 >> ps)
    where ps = do
                v <- get
                if null v || null (head v)
                    then takeN 1 >> return []
                    else liftM2 (:) (parseCoord . head <$> takeN 1) ps


parseScanners :: State [String] [Space]
parseScanners = do
    s <- get
    if null s
        then return []
        else liftM2 (:) parseScanner parseScanners

parse :: String -> [Space]
parse = evalState parseScanners . lines

-------------------------------------------------------
-- Vector operations

diff :: Pt -> Pt -> Vec
(x, y, z) `diff` (x', y', z') = (x - x', y - y', z - z')

translate :: Vec -> Pt -> Pt
translate (dx, dy, dz) (x, y, z) = (x + dx, y + dy, z + dz)

neg :: Vec -> Vec
neg (x, y, z) = (-x, -y, -z)

-- aka manhattan norm
taxicab :: Vec -> Int
taxicab (x, y, z) = abs x + abs y + abs z

------------------------------------------------------
--  Rotation

type Rotation = Pt -> Pt
transforms :: [Rotation]
transforms = [
    -- z up
    id, -- e.g. don't rotate
    \(x, y, z) -> (-y, x, z),
    \(x, y, z) -> (-x, -y, z),
    \(x, y, z) -> (y, -x, z),
    -- -z up
    \(x, y, z) -> (x, -y, -z),
    \(x, y, z) -> (y, x, -z),
    \(x, y, z) -> (-x, y, -z),
    \(x, y, z) -> (-y, -x, -z),
    -- y up
    \(x, y, z) -> (x, -z, y),
    \(x, y, z) -> (z, x, y),
    \(x, y, z) -> (-x, z, y),
    \(x, y, z) -> (-z, -x, y),
    -- -y up
    \(x, y, z) -> (x, z, -y),
    \(x, y, z) -> (-z, x, -y),
    \(x, y, z) -> (-x, -z, -y),
    \(x, y, z) -> (z, -x, -y),
    -- x up
    \(x, y, z) -> (-z, y, x),
    \(x, y, z) -> (-y, -z, x),
    \(x, y, z) -> (z, -y, x),
    \(x, y, z) -> (y, z, x),
    -- -x up
    \(x, y, z) -> (z, y, -x),
    \(x, y, z) -> (-y, z, -x),
    \(x, y, z) -> (-z, -y, -x),
    \(x, y, z) -> (y, -z, -x)
 ]

------------------------------------------------------
--  Alignment

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (a : as) = Just a

-- Determine a vector which translates at least 12 points from
-- the first space to the second space.
getTranslationVec :: Space -> Space -> Maybe Vec
getTranslationVec s1 s2 = let
    vecs = S.map (uncurry diff) (S.cartesianProduct s2 s1) 
    in safeHead $ S.toList (S.filter (\v -> numAlign v s1 s2 >= 12) vecs)
    where
        numAlign v s1 s2 = let
            s1' = S.map (translate v) s1
            in S.size (S.intersection s1' s2)

-- Determine a rotation and translation which will transform
-- s2 so it shares the same reference axis of s1.
align :: Space -> Space -> Maybe (Rotation, Vec)
align s1 s2 = safeHead $ mapMaybe findAfterRotate transforms
    where
        findAfterRotate r = (r,) <$> getTranslationVec s1 (S.map r s2)

match :: Space -> M.Map Int Space -> Maybe (Vec, Space, M.Map Int Space)
match s ss = 
    case mapMaybe (\(i, s') -> (i,s',) <$> align s s') (M.assocs ss) of
        (i, s', (r, v)) : _ -> traceShow i $ Just (
            neg v, S.map (translate (neg v) . r) s', M.delete i ss
            )
        _ -> Nothing

matchAll :: Space -> [Space] -> ([Pt], Space)
matchAll s0 ss = let
    res = _ma [((0,0,0),s0)] $ M.fromList (zip [0..] ss)
    in foldr step ([], S.empty) res
    where
        _ma sa sn 
          | not (null sn) = case mapMaybe ((`match` sn) . snd) sa of
                (v, s', sn') : _ -> _ma ((neg v, s'):sa) sn'
                [] -> error "can't map all"
          | otherwise = sa
        
        step (v, s) = bimap (v:) (S.union s)

main = do
    contents <- getArgs >>= readFile . head

    let (s:ss) = parse contents
    let (vs, s') = matchAll s ss
    
    mapM_ print  vs
    print "----"
    print $ S.size s'

    let ds = [taxicab (diff v1 v2) | v1 <- vs, v2 <- vs]
    print $ maximum ds
