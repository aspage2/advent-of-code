import Control.Monad
import Control.Monad.Trans.State
import qualified Data.Char as Char
import System.Environment
import Numeric
import Common.Parse

import Data.Bifunctor

{- Day 16 - Packet Protocols 

By parsing the bit sequence and forming a syntax tree, it's easy to compute the version sums
or implement the operations.

To build a parser, we can gradually consume elements of the bitstring and interpret them as 
we need. Gradually reducing the bit sequence works well with the State monad. For example,
a State monad representing taking the first 3 bits would be:

take3 :: State String String
take3 = state $ (\s -> splitAt 3 s)

Which we can utilize with runState:

>>> runState take3 "10110010"
("101", "10010")

Seems kind of indirect, but what we get from handling state through the State monad is
the ability to compose different State functions using monadic functions and `do`

take3Twice :: State String [String]
take3Twice = do
    v1 <- take3
    v2 <- take3
    return [v1, v2]

take3FourTimes :: State String [String]
take3FourTimes = do
    v1 <- take3Twice
    v2 <- take3Twice
    return v1 ++ v2

>>> runState take3Twice "101111000110"
(["101", "111"], "000110")
>>> runState take3FourTimes "101111000110"
(["101", "111", "000", "110"], "")
-}

toBitString :: String -> String
toBitString = concatMap (step . Char.toLower)
    where step '0' = "0000"
          step '1' = "0001"
          step '2' = "0010"
          step '3' = "0011"
          step '4' = "0100"
          step '5' = "0101"
          step '6' = "0110"
          step '7' = "0111"
          step '8' = "1000"
          step '9' = "1001"
          step 'a' = "1010"
          step 'b' = "1011"
          step 'c' = "1100"
          step 'd' = "1101"
          step 'e' = "1110"
          step 'f' = "1111"
          step _ = undefined 

c2i :: Char -> Int
c2i '1' = 1
c2i '0' = 0
c2i _ = undefined 

asInt :: String -> Int
asInt s = let l = length s 
          in sum $ zipWith (\i v -> c2i v * 2 ^ (l - i - 1)) [0..] s

takeNum :: Int -> State String Int
takeNum n = asInt <$> takeN n

takeLiteralPayload :: State String (Int, Int)
takeLiteralPayload = second asInt <$> _t
    where _t = do
            bts <- takeN 5
            let t = tail bts
            if head bts == '0'
                then return (5, t)
                else bimap (+5) (t++) <$> _t 

takeOperatorPayload :: State String (Int, [Packet])
takeOperatorPayload = do
    typeId <- head <$> takeN 1
    if typeId == '0'
        then do
            nb <- takeNum 15
            ps <- _tpb nb
            return (16 + nb, ps)
        else do
            np <- takeNum 11
            ps <- replicateM np takePacket
            return $ foldr step (12, []) ps
    where
        step (n, p) (tot, ps) = (tot + n, p : ps)
        _tpb n | n <= 0 = return [] 
               | otherwise = do
                   (n', p) <- takePacket
                   (p:) <$> _tpb (n - n')
            

verSum :: Packet -> Int
verSum (Literal v _) = v
verSum (Operator v _ ps) = v + sum (map verSum ps)


evaluate :: Packet -> Int
evaluate (Literal _ val) = val
evaluate (Operator _ op ps) | op == 0 = agg sum ps
                            | op == 1 = agg product ps
                            | op == 2 = agg minimum ps
                            | op == 3 = agg maximum ps
                            | op == 5 = chk (>) ps
                            | op == 6 = chk (<) ps
                            | op == 7 = chk (==) ps
                            | otherwise = undefined
    where
        chk op ps = if evaluate (ps !! 0) `op` evaluate (ps !! 1) then 1 else 0
        agg op = op . map evaluate

data Packet = Literal Int Int | Operator Int Int [Packet] deriving (Show)

takePacket :: State String (Int, Packet)
takePacket = do
    ver <- takeNum 3
    typ <- takeNum 3
    if typ == 4
        then bimap (+6) (Literal ver) <$> takeLiteralPayload
        else bimap (+6) (Operator ver typ) <$> takeOperatorPayload

parse :: String -> Packet
parse = snd . evalState takePacket

interactiveLoop :: IO ()
interactiveLoop = do
    l <- getLine 
    if l == ""
        then return ()
        else let pkt = parse (toBitString l) in print pkt >> print (evaluate pkt) >> interactiveLoop

main = do
    contents <- getArgs >>= readFile . head

    mapM_ (print . (\l -> (l, evaluate (parse . toBitString $ l)))) $ lines contents
