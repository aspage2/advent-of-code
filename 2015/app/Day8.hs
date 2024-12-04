{-# LANGUAGE LambdaCase #-}
module Day8 where

import Control.Applicative ((<|>))
import Parser 
import Control.Monad (liftM2, forM_)
import Data.Char (chr, isHexDigit, toLower, ord)
import Data.List (foldl')

data Esc = Quote | BS | Hex !Int deriving (Show)

readHex :: (Char -> Char -> Either String Int)
readHex c1 c2 | not $ isHexDigit c1 = Left "left char is not hex"
              | not $ isHexDigit c2 = Left "right char is not hex"
              | otherwise = Right $ 16 * ti c1 + ti c2
              where ti c | ord c >= ord '0' && ord c <= ord '9' = ord c - ord '0'
                         | otherwise = ord (toLower c) - ord 'a'
                         
twoDigitInt :: Parser Int
twoDigitInt = do
    c <- anyChar
    c' <- anyChar
    case readHex c c' of
        Right x -> return x
        Left err -> parseError err

escape :: Parser Char
escape = (str "\\\\" >> return '\\')
         <|> (str "\\\"" >> return '"')
         <|> (str "\\x" >> (chr <$> twoDigitInt))

line :: Parser String
line = many (escape <|> notPred (=='"')) <|> return []

quotedLine :: Parser String
quotedLine = do
    char '"'
    xs <- line 
    char '"'
    return xs

many :: Parser a -> Parser [a]
many p = rec <|> return []
    where rec = liftM2 (:) p (many p)

decode :: String -> Either Error String
decode s = fst <$> runParser quotedLine s

encode :: String -> String
encode s = s >>= \case
    '\\' -> "\\\\"
    '"' -> "\\\""
    c -> [c]

dayMain :: String -> IO ()
dayMain fname = do
    ls <- lines <$> readFile fname
    forM_ (take 5 ls) $ \l -> do
        putStr l
        putChar ' '
        putStrLn (encode l)


    let (dec, enc, tot) = foldl' f (0, 0, 0) ls
    print $ tot - dec
    print $ enc - tot

    where f (d, e, t) s = case decode s of
                            Right res -> (d + length res, e + length (encode s) + 2, t + length s)
                            Left err -> error $ show err

