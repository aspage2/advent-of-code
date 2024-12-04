
module Day10 where

import Control.Monad (mapM_)
import Parser

inputData :: String 
inputData = "3113322113"

theThing :: Parser String
theThing = do
    d <- curr
    case d of
        "" -> return ""
        x:_ -> do
            n <- length <$> while (==x)
            ((show n ++ [x]) ++) <$> theThing 

yes :: Either b a -> a
yes (Left _) = error "left"
yes (Right r) = r

dayMain :: String -> IO ()
dayMain _ = do
    let em = iterate (fst . yes . runParser theThing) inputData

    print $ length $ em !! 50
