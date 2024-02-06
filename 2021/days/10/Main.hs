import Data.List

import Data.Maybe
import System.Environment


{- Day 10 - Brackets

For bracket-matching, the most recently-opened bracket type is the one which should
be closed first. e.g. since '[' is the last bracket to open in "(<()[", we should expect
either an open bracket next or ']'. If we see ']', the next most recent opened-bracket was
'<', so we should expect to see any open bracket or '>' next.

This problem has a first-in, last-out (FILO) behavior. Consequently, we can use a stack
data structure to validate a string in linear time:
--------- imperative algorithm:

start with an empty stack.

for each bracket in the string:

    if it's an opening bracket:
        push the bracket type onto the stack.

    else (it's a closing bracket):
        pop an entry from the top of the stack. If the stack is empty, 
          the string has an extra closing bracket.

        if that entry isn't the same type as the current char, 
          the bracket is corrupted at the current position.

if we exit the loop with a nonempty stack, that means the string is
  missing closing brackets in the order of the stack.
-}

conv :: Char -> Char
conv '[' = ']'
conv '(' = ')'
conv '{' = '}'
conv '<' = '>'
conv _ = undefined 

match :: Char -> Char -> Bool
match co cc = cc == conv co

firstIllegalChar :: String -> Maybe Char
firstIllegalChar s = _check s []
    where _check [] _ = Nothing
          _check (c:cs) bs = case c of
              c' | elem c' ['[', '(', '{', '<'] -> _check cs (c : bs)
                 | elem c' [']', ')', '}', '>'] -> if _c bs c then Just c else _check cs (tail bs)
                 | otherwise -> Just c
          _c [] _ = True
          _c (b':_) c' = not $ match b' c' 

complete :: String -> String
complete = map conv . foldl _complete []
    where
        _complete bs c = case c of
              c' | elem c' ['[', '(', '{', '<'] -> c' : bs
                 | elem c' [']', ')', '}', '>'] -> drop 1 bs
                 | otherwise -> undefined 

score :: Char -> Int
score ')' = 3
score ']' = 57
score '}' = 1197
score '>' = 25137
score _ = undefined 

total :: [Char] -> Int
total = foldr (\c acc -> acc + score c) 0

score2 :: String -> Int
score2 = foldl step 0
    where step acc c = 5 * acc + val c
          val ')' = 1
          val ']' = 2
          val '}' = 3
          val '>' = 4
          val _ = undefined 

main = do
    contents <- lines <$>( getArgs >>= readFile . head)

    print $ total $ mapMaybe firstIllegalChar contents

    let scores = map (score2 . complete) $ filter (isNothing . firstIllegalChar) contents
    let l = length scores
    let ss = sort scores

    print $ ss !! (l `div` 2)