module Main where
import Data.Maybe (isNothing, fromMaybe)
import Text.Read (readEither)
import System.Environment (getArgs)

import Days (selectDay)


data Options = Options {
    day :: !(Maybe Int),
    fname :: !String
} deriving (Show)

setDay :: Options -> Int -> Options
setDay opt x = opt { day = Just x }

parse :: Options -> [String] -> Either String Options
parse opt [] = Right opt
parse opt args = case args of
    -- Options
    x : y : rest | x == "--day" ->  do
                    day' <- readEither y
                    parse (setDay opt day') rest
                 | x == "--file" -> parse (opt { fname = y }) rest
                 | otherwise -> Left ("Unknown value: " ++ x)
    _else -> Left "Could not parse"

guard :: (a -> Bool) -> String -> a -> Either String a
guard predicate errMsg x | predicate x = Left errMsg
                         | otherwise = Right x

parseArgs :: [String] -> Either String Options
parseArgs args = do 
    opts <- parse defaultOptions args 
    guard (isNothing . day) "day not specified" opts
    where defaultOptions = Options Nothing "input.txt"

main :: IO ()
main = do
    args <- getArgs

    let res = do
         opts <- parseArgs args
         let dayN = fromMaybe 0 (day opts)
         dayE <- selectDay dayN
         return ("data/Day"++show dayN++"/"++fname opts, dayE)

    case res of
        Left err -> putStrLn err
        Right (loc, dayE) -> dayE loc





