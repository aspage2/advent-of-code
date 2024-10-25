{-# LANGUAGE OverloadedStrings #-}

module Day4 where

import Control.Monad.Fix (fix)

import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BD
import qualified Data.ByteString as BD
import qualified Data.ByteString.Builder as BS

base :: MD5.Ctx
base = MD5.update MD5.init "bgvyzdsv"

counter :: [Int]
counter = fix (\f n -> n : f (n + 1)) 1

hsh :: Int -> BS.ByteString
hsh x = let 
    toStr = BD.toStrict $ BD.toLazyByteString (BD.intDec x)
    in BD.toStrict $ BS.toLazyByteString $ BS.byteStringHex $ MD5.finalize (MD5.update base toStr)

dayMain :: String -> IO ()
dayMain _ = let
    hashed = map (\b -> (hsh b, b)) counter
    a = snd . head $ filter (BS.isPrefixOf "00000" . fst) hashed
    b = snd . head $ filter (BS.isPrefixOf "000000" . fst) hashed
    in print a >> print b

