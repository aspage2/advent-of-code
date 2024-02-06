import Data.Maybe
import Debug.Trace
import Data.List
import Data.Bifunctor
import System.Environment

{- Day 17 - yeeting probes

Because the x and y accelerations of the probe are independant, we can
reason about the probe velocities separately.

We can do a search with a reduced test set by finding lower & upper bounds
for vx and vy.

The maximum velocity for VX is the rightmost X value in the target bounds.
any higher and we will immediately shoot past the target.

The same logic applies to minimum Y velocity: choose the bottom Y coordinate
of the target's bounds.

For min X, we must choose a VX such that by the time VX ends up at 0, we 
are at least past the left boundary of the target. This is the same as
saying l <= vx + (vx - 1) + (vx - 2) + ... + 1 = vx (vx + 1) / 2. This is 
a quadratic equation which we can solve with the quadratic formula. We will
often get a non-whole number for vx, so we must take the ceiling of the result
to get the smallest vx which is at least the solution to the equation.

For max Y, realize that for any initial velocity VY > 0, the probe will have a
step where its Y coordinate is 0 and its y velocity = -(VY + 1). Therefore, for
y_lower being the bottom of the target, we can't shoot higher than |VY| - 1.

With those bounds defined, we can simply test each inital velocity to see if a
step lands.
-}

data Probe = Probe { pos :: Cell, velocity :: Vec } deriving (Show)
type Bounds = (Cell, Cell)
type Cell = (Int, Int)
type Vec = (Int, Int)

within :: Bounds -> Cell -> Bool
within ((r0, r1), (c0, c1)) (r, c) = r0 <= r && r <= r1 && c0 <= c && c <= c1

step :: Probe -> Probe
step p = let
    (r, c) = pos p
    (vr, vc) = velocity p
    pos' = (r + vr, c + vc)
    vel' = (vr - 1, vc - signum vc)
    in Probe pos' vel' 

landingCell :: Bounds -> Vec -> Bool
landingCell bs vi = _ww (launch vi)
    where
        _ww probe | within bs (pos probe) = True 
                  | past bs (pos probe) = False
                  | otherwise = _ww (step probe)


past :: Bounds -> Cell -> Bool
past ((r1, _), (_, c1)) (r, c) = r < r1 || c > c1

solveVX :: (Integral a, Integral b) => a -> b
solveVX x = let fx = fromIntegral x :: Float
            in ceiling $ (sqrt (1 + 8 * fx) - 1) / 2

minVX :: Bounds -> Int
minVX (_, (c0, c1)) = minimum $ map solveVX [c0..c1]

parse :: String -> Bounds
parse s = let
    ws = words s
    crange = drop 2 (ws !! 2)
    rrange = drop 2 (ws !! 3)
    (c0, rest) = first read (break (=='.') crange)
    c1 = read (takeWhile (/=',') . drop 2 $ rest)
    (r0, rest') = first read (break (=='.') rrange)
    r1 = read (drop 2 rest')
    in ((r0, r1), (c0, c1))


allSolutions :: Bounds -> [Vec]
allSolutions bs = let
    minvx = minVX bs
    maxvx = snd (snd bs)
    minvy = fst (fst bs)
    maxvy = abs (fst (fst bs))
    attempts = [(vr, vc) | vr <- [minvy..maxvy], vc <- [minvx..maxvx]]
    in traceShow (minvx, maxvx, minvy, maxvy) $ filter (landingCell bs) attempts


launch :: Vec -> Probe
launch = Probe (0, 0)

main = do
    bounds <- parse <$> (getArgs >>= readFile . head)
    print $ length $ allSolutions bounds
