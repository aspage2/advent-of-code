{-# LANGUAGE  LambdaCase #-}
module Day12 where

import Control.Applicative ((<|>))

import Parser

data JSON = 
      Obj ![(String, JSON)]
    | Arr ![JSON]
    | Num !Int
    | Str !String
    deriving (Show, Eq)

sepBy :: Char -> Parser a -> Parser [a]
sepBy c p = do 
    whiteSpace
    v <- p
    whiteSpace
    (char c >> (v:) <$> sepBy c p) <|> return [v]

jsonObj :: Parser JSON
jsonObj = Obj <$> do
    char '{'
    whiteSpace
    l <- sepBy ',' $ do
        s <- string
        whiteSpace >> char ':' >> whiteSpace
        obj <- parseIt
        whiteSpace
        return (s, obj)
    whiteSpace
    char '}'
    return l

string :: Parser String
string = do
    char '"'
    x <- while (/= '"')
    char '"'
    return x

jsonString :: Parser JSON
jsonString = Str <$> string

jsonNum :: Parser JSON
jsonNum = do
    c <- head <$> curr
    Num <$> case c of
        '-' -> (\x -> -x) <$> (char '-' >> integer)
        _ -> integer

jsonArray :: Parser JSON
jsonArray = Arr <$> do
    char '[' >> whiteSpace
    l <- sepBy ',' $ do
        whiteSpace
        val <- parseIt
        whiteSpace
        return val
    whiteSpace >> char ']'
    return l

parseIt :: Parser JSON
parseIt = jsonObj <|> jsonString <|> jsonNum <|> jsonArray

sumIt :: JSON -> Int
sumIt (Str _) = 0
sumIt (Num x) = x
sumIt (Arr xs) = sum (map sumIt xs)
sumIt (Obj xs) = sum (map (sumIt . snd) xs)

sumIt2 :: JSON -> Int
sumIt2 (Obj xs) = if any (\case
                            (_, Str "red") -> True
                            _else       -> False) xs
                  then 0
                  else sum (map (sumIt2 . snd) xs)
sumIt2 (Arr xs) = sum (map sumIt2 xs)
sumIt2 v = sumIt v

dayMain :: String -> IO ()
dayMain fname = do
    obj <- runParser parseIt <$> readFile fname 

    case obj of
        Left e -> print e
        Right (r, _) -> print (sumIt r) >> print (sumIt2 r)

