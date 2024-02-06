module Common.IO where 

import System.Environment ( getArgs )

firstArg :: IO String
firstArg = head <$> getArgs 

getInts :: String -> IO [Int]
getInts = (fmap . fmap) (map (read :: String -> Int) . lines) readFile -- wtf, haskell
