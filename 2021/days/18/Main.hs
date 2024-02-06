
import Data.Maybe
import Common.Parse
import Data.Char
import Control.Monad.Trans.State
import System.Environment

{- Day 18 - Snailfish

Binary trees for dayz.


-}

-- Parse
data Number = Literal Int | Pair Number Number deriving (Eq)

instance Show Number where
    show (Pair l r) = "[" ++ show l ++ "," ++ show r ++ "]"
    show (Literal x) = show x

takeLiteral :: State String Number
takeLiteral = Literal . read <$> _tl
    where _tl = do
            c <- head <$> get
            if isNumber c
                then (:) <$> (head <$> takeN 1) <*> _tl
                else return []

takePair :: State String Number
takePair = do
    takeN 1
    v1 <- takeNumber
    takeN 1
    v2 <- takeNumber
    takeN 1
    return (Pair v1 v2)

takeNumber :: State String Number
takeNumber = do
    c <- head <$> get
    if isNumber c
        then takeLiteral
        else takePair

parse :: String -> Number
parse = evalState takeNumber

-- Zipper
data Ctx = Top | L Ctx Number | R Number Ctx deriving (Show, Eq)
type Loc = (Number, Ctx)

left :: Loc -> Maybe Loc
left (Pair l r, c) = Just (l, L c r)
left _ = Nothing

right :: Loc -> Maybe Loc
right (Pair l r, c) = Just (r, R l c)
right _ = Nothing

top :: Number -> Loc
top t = (t, Top)

up :: Loc -> Maybe Loc
up (t, L c r) = Just (Pair t r, c)
up (t, R l c) = Just (Pair l t, c)
up _ = Nothing

upmost :: Loc -> Loc
upmost l@(t, Top) = l
upmost l = upmost (fromJust $ up l)

modify :: Loc -> (Number -> Number) -> Loc
modify (t, c) f = (f t, c)

-- Complex zipper functions

-- Find the sibling-leaf to the left of the current location.
leftSibling :: Loc -> Maybe Loc
leftSibling loc = rightmostChild <$> (_up loc >>= up >>= left)
    where
        _up loc@(_, R _ _) = Just loc
        _up loc@(_, L _ _) = up loc >>= _up
        _up (_, Top) = Nothing

-- Find the sibling-leaf to the right of the current location
rightSibling :: Loc -> Maybe Loc
rightSibling loc = leftmostChild <$> (_up loc >>= up >>= right)
    where
        _up loc@(_, L _ _) = Just loc
        _up loc@(_, R _ _) = up loc >>= _up
        _up (_, Top) = Nothing

-- Find the rightmost literal in the current location
rightmostChild :: Loc -> Loc
rightmostChild l = maybe l rightmostChild (right l)

-- Find the leftmost literal in the current location
leftmostChild :: Loc -> Loc
leftmostChild loc = maybe loc leftmostChild (left loc)

-- Literal-plus - add n to the value of a literal.
lp :: Int -> Number -> Number
lp n (Literal x) = Literal (x + n)
lp _ _ = undefined

-- To explode a pair [x,y] (x and y are literals), add x to the
-- left-sibling of the pair and add y to the right-sibling of the
-- pair. Skip a sibling if it doesn't exist.
explode :: Loc -> Maybe Loc
explode loc@(Pair (Literal l) (Literal r), _) = let
    loc' = Main.modify loc (const $ Literal 0)
    res = explodel loc' >>= rightSibling
    loc'' = fromMaybe loc' res
    res' = exploder loc'' >>= leftSibling
    loc''' = fromMaybe loc'' res'
    in if isNothing res && isNothing res' then Nothing else Just loc'''
    where
        explodel loc =
            (`Main.modify` lp l) <$> leftSibling loc
        exploder loc =
            (`Main.modify` lp r) <$> rightSibling loc
explode _ = undefined

-- Split a literal number into a new pair of [floor (x / 2), ceil (x / 2)]
split :: Loc -> Loc
split loc = Main.modify loc (\(Literal x) ->
    Pair (Literal $ x `div` 2) (Literal $ x `div` 2 + (x `mod` 2)))

-- The magnitude of a given snailfish number.
magnitude :: Number -> Int
magnitude (Literal x) = x
magnitude (Pair l r) = 3 * magnitude l + 2 * magnitude r

-- Return the location of the leftmost pair nested at least 4
-- pairs deep in the tree.
pairAtLeast4Deep :: Loc -> Maybe Loc
pairAtLeast4Deep = _s 0
    where _s n loc@(Pair (Literal _) (Literal _), _) =
            if n < 4 then Nothing else Just loc
          _s n loc@(Pair l r, _) = let
              lc = _s (n + 1) (fromJust $ left loc)
              in if isNothing lc
                    then _s (n + 1) (fromJust $ right loc)
                    else lc
          _s n (Literal _, _) = Nothing

-- Return the leftmost number that is greater than 9 in the
-- tree, if it exists.
splittableNumber :: Loc -> Maybe Loc
splittableNumber loc@(Literal x, _) | x > 9 = Just loc
                                    | otherwise = Nothing
splittableNumber loc@(Pair _ _, _) = let
    v = left loc >>= splittableNumber
    in if isNothing v then right loc >>= splittableNumber else v

-- Reduce a snailfish number into its simplest form by applying
-- explode and split operations until they can't be applied any more.
-- explodable pairs take precedence over any splittable literals.
reduce :: Number -> Number
reduce t = fst . upmost $ _r (top t)
    where
        _r l = case pairAtLeast4Deep l >>= explode of
            Just loc -> _r (upmost loc)
            Nothing -> maybe l (_r . upmost) (splittableNumber l >>= Just . split)

-- Sum of a list of snailfish numbers.
numSum :: [Number] -> Number
numSum nums = _ns (head nums) (tail nums)
    where _ns x (y:ys) = _ns (reduce $ Pair x y) ys
          _ns v [] = v

-- The largest magnitude sum of a list of snailfish numbers.
biggestMagnitude :: [Number] -> Int
biggestMagnitude ns = let
    pairs = [Pair x y | x <- ns, y <- ns, x /= y]
    in maximum (map (magnitude . reduce) pairs)

main = do
    contents <- getArgs >>= readFile . head
    let ns = map parse . lines $ contents

    print $ biggestMagnitude ns
