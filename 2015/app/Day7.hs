module Day7 where

import           Parser (Parser (..), integer, while, str, whiteSpace, remaining)
import           Control.Applicative ((<|>))
import           Control.Monad (liftM3)
import qualified Data.Map as M
import           Data.Bits ((.&.), (.|.), shiftL, shiftR, complement)

data BinOp = AND | OR | LSHIFT | RSHIFT deriving (Show)

newtype NotExp = NotOp Primary deriving (Show)

data Primary = Literal !Int | Symbol !String deriving (Show)

data Expression = 
      Value !Primary
    | Not !Primary
    | Bin !Primary !BinOp !Primary
    deriving (Show)

doBinOp :: BinOp -> Int -> Int -> Int
doBinOp AND = (.&.)
doBinOp OR = (.|.)
doBinOp LSHIFT = shiftL
doBinOp RSHIFT = shiftR


eval :: Expression -> M.Map String Expression -> Maybe Int
eval e m = case e of 
    Value p -> getLit p
    Not p -> complement <$> getLit p
    Bin l op r -> do
        l' <- getLit l
        r' <- getLit r
        Just $ doBinOp op l' r'

    where getLit (Literal x) = Just x
          getLit (Symbol s)  = case M.lookup s m of
                                Just (Value (Literal x)) -> Just x
                                _else -> Nothing 

step :: M.Map String Expression -> M.Map String Expression
step m = M.map (\e -> maybe e (Value . Literal) (eval e m)) m

solve :: M.Map String Expression -> Int
solve m = case M.lookup "a" m of
    Nothing -> error "well shit!"
    Just (Value (Literal x)) -> x
    _else -> solve $ step m

operand :: Parser Primary
operand = (Literal <$> integer) <|> (Symbol <$> while (/=' '))

binOp :: Parser BinOp
binOp =  (str "AND" >> return AND)
     <|> (str "OR"  >> return OR)
     <|> (str "LSHIFT" >> return LSHIFT)
     <|> (str "RSHIFT" >> return RSHIFT)

binExp :: Parser Expression
binExp = liftM3 Bin operand (whiteSpace >> binOp) (whiteSpace >> operand)

notExp :: Parser Expression
notExp = Not <$> (str "NOT" >> whiteSpace >> operand)

expression :: Parser Expression
expression = binExp <|> notExp <|> (Value <$> operand)

statement :: Parser (String, Expression) 
statement = do
    expr <- expression
    _<-whiteSpace
    str "->"
    _<-whiteSpace
    sym <- remaining
    return (sym, expr)

results :: [Either a (b, c)] -> [b]
results [] = []
results (x:xs) = case x of
    Right a -> fst a : results xs
    _left -> results xs

-------------------------------------------------------

dayMain :: String -> IO ()
dayMain fname = do
    ls <- results . map (runParser statement) . lines <$> readFile fname 

    let ini = M.fromList ls

    let p1 = solve ini

    print $ "Part1: " ++ show p1
    print $ "Part2: " ++ show (solve (M.insert "b" (Value (Literal p1)) ini))


