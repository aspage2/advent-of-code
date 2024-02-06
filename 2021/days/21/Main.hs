import qualified Data.Map as M
import System.Environment
import Data.Maybe

data PlayerState = PlayerState { position :: Integer, score :: Integer}

nextPlayerState :: Integer -> PlayerState -> PlayerState
nextPlayerState i (PlayerState pos scr) = let
    pos' = (pos + i) `mod` 10
    scr' = scr + pos' + 1
    in PlayerState pos' scr'

instance Show PlayerState where
    show (PlayerState pos scr) = "(" ++ show pos ++ ", " ++ show scr ++ ")"

data GameState = GameState {
    p1 :: PlayerState,
    p2 :: PlayerState,
    die :: Integer,
    turn :: Bool
}

instance Show GameState where
    show gs = concat [
        (if turn gs then "P1*" else "P1") ++ show (p1 gs),
        " ",
        (if not (turn gs) then "P2*" else "P2") ++ show (p2 gs)
        ]

newGameState :: Integer -> Integer -> GameState
newGameState p1 p2 = GameState (PlayerState p1 0) (PlayerState p2 0) 1 True

nextState :: GameState -> GameState
nextState gs = let
    roll = 3 * (die gs + 1)
    in if turn gs
        then gs {p1 = nextPlayerState roll (p1 gs), turn = False, die = (die gs + 3) `mod` 100}
        else gs {p2 = nextPlayerState roll (p2 gs), turn = True, die = (die gs + 3) `mod` 100}

winnerP :: PlayerState -> Bool
winnerP p = score p >= 1000

winner :: GameState -> Maybe Bool
winner gs
  | (score . p1) gs >= 1000 = Just True
  | (score . p2) gs >= 1000 = Just False
  | otherwise = Nothing

example :: (Integer, Integer)
example = (3, 7)

part1 = (7, 5)

part1digest :: GameState -> Integer
part1digest gs = let
    (n, gsf) = last . takeWhile (isNothing . winner . snd) $ zip [1..] (iterate nextState gs) 
    gsf' = nextState gsf
    in n * 3 * min (score $ p1 gsf') (score $ p2 gsf')

rollsOf3 :: [(Integer, Integer)]
rollsOf3 = let
    rolls = [i + j + k | i <- [1..3], j <- [1..3], k <- [1..3]]
    in M.toList $ foldr step M.empty rolls
    where
        step s m = M.insertWith (+) s 1 m

{-
G(s) = G(roll(3,s)) + 3*G(roll(4,s)) + 6*G(roll(5,s)) + 7*G(roll(6,s)) + 6*G(roll(7,s)) + 3*G(roll(8,s)) + G(roll(9,s))
-}

countPossibleWins :: (PlayerState, PlayerState) -> (Integer, Integer)
countPossibleWins = _c True
    where 
        _c b (p1, p2) 
         | winnerP p1 = (1, 0)
         | winnerP p2 = (0, 1)
         | otherwise = foldr (step b (p1, p2)) (0, 0) rollsOf3
        step b (p1, p2) (roll, ct) (p1s, p2s) = let
            gs' = if b 
                then (nextPlayerState roll p1, p2)
                else (p1, nextPlayerState roll p2)
            (p1s', p2s') = _c (not b) gs'
            in (ct * p1s', ct * p2s')
        
main = do
    let gs = uncurry newGameState example

    print $ part1digest gs
    print $ countRollsOf3
