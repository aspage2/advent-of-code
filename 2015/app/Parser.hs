{-# LANGUAGE LambdaCase #-}

module Parser where

import Control.Applicative (Alternative, (<|>), empty)
import Data.Function (fix)
import Data.Char (isDigit)
import Text.Read (readEither)

data Error = Error !String | EOF | Nil deriving (Show)

newtype Parser a = Parser { 
    runParser :: String -> Either Error (a, String)
}

------------------------------------------------------------

instance Functor Parser where
    fmap f (Parser rp) = Parser $ \s -> case rp s of
        Right (a, rest) -> Right (f a, rest)
        Left e -> Left e

instance Applicative Parser where 

    pure x = Parser $ \s -> Right (x, s)

    (Parser r) <*> (Parser r') = Parser $ \s -> do
        (f, rest) <- r s
        (a, rest') <- r' rest
        return (f a, rest')

instance Monad Parser where

    (Parser r) >>= f = Parser $ \s -> do
        (a, rest) <- r s
        runParser (f a) rest

instance Alternative Parser where

    empty = Parser $ \_ -> Left Nil

    (Parser r) <|> (Parser r') = Parser $ \s -> case r s of
        Right x -> Right x
        Left _ -> case r' s of
            Right x -> Right x
            Left err -> Left err

------------------------------------------------------------

char :: Char -> Parser ()
char c = Parser $ \case
    []                 -> Left EOF
    (x:xs) | c == x    -> Right ((), xs)
           | otherwise -> Left (Error $ "expected " ++ show c)

str :: String -> Parser ()
str pref = Parser $ _prefix pref
    where
        _prefix [] rest                     = Right ((), rest)
        _prefix _ []                        = Left EOF
        _prefix (p:ps) (r:rest) | p == r    = _prefix ps rest
                                | otherwise = Left (Error "mismatched prefix")

while :: (Char -> Bool) -> Parser String
while pr = Parser $ fix $ \rec s -> case s of
    [] -> Right ([], [])
    xss@(x:xs) | pr x      -> modFst (x:) <$> rec xs
               | otherwise -> Right ([], xss)
    where modFst f (a, b) = (f a, b)

whiteSpace :: Parser String
whiteSpace = while (==' ')

parseError :: String -> Parser a
parseError e = Parser $ const $ Left $ Error e

integer :: Parser Int
integer = do
    lexeme <- while isDigit
    case readEither lexeme of
        Left err -> parseError err
        Right i -> return i

remaining :: Parser String
remaining = Parser $ \s -> Right (s, [])
