module Common.Parse where

import Control.Monad.Trans.State ( state, State )

takeN :: Eq a => Int -> State [a] [a]
takeN n = state $ splitAt n

trimN :: Int -> State [a] () 
trimN n = state $ \s -> ((), drop n s)

trimLeft :: Eq a => a -> State [a] ()
trimLeft c = state $ \s -> ((), dropWhile (==c) s)

delim :: Eq a => a -> State [a] [a]
delim c = state $ \s -> let (fst, rest) = break (==c) s in (fst, drop 1 rest)
