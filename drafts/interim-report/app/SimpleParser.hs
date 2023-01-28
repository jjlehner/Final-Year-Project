module SimpleParser where

import Control.Applicative
import Control.Monad
import qualified Data.Map as Map
import Data.Function ( (&) )
import Data.Char
import Control.Monad ()
import Debug.Trace
import Control.Monad.State.Strict

newtype Parser a = Parser (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (Parser p) inp = p inp

item :: Parser Char
item = Parser (\inp -> case inp of
                             [] -> []
                             (x:xs) -> [(x,xs)])

instance Functor Parser where
    fmap g p = Parser (\inp -> case parse p inp of
                                []        -> []
                                [(v,out)] -> [(g v, out)])

instance Applicative Parser where
    pure v = Parser (\inp -> [(v, inp)])

    pg <*> px = Parser (\inp -> case parse pg inp of
                                    []        -> []
                                    [(g,out)] -> parse (fmap g px) out)

instance Monad Parser where
    p >>= f = Parser (\inp -> case parse p inp of
                                    [] -> []
                                    [(v,out)] -> parse (f v) out
                     )

instance Alternative Parser where
    empty = Parser (\inp -> [])
    p <|> q = Parser (\inp -> case parse p inp of
                                [] -> parse q inp
                                [(v,out)] -> [(v,out)]
                     )

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then
            return x
           else
            empty

letter = sat isAlpha
char x = sat (==x)
string [] = return []

string (x:xs) = do char x
                   string xs
                   many space
                   return (x:xs)
space = char ' '
digit = sat isNumber

lexeme g =
    do  res <- g
        many space
        return res

identifierDeclaration =
    do string "let"
       name <- lexeme (many letter)
       lexeme $ char '='
       value <- lexeme (many digit)
       return ()

data Expr =
    EBinary{
        term :: Term,
        expr :: Expr
    } | EUnary Term deriving (Show)

data Term =
    TBinary {
        factor :: Factor,
        term :: Term
    } | TUnary Factor deriving (Show)

data Factor = TExpr Expr | TNat Int deriving (Show)

number = do num <- lexeme (many digit)
            pure $ (read num :: Int)

symbol x = lexeme $ char x

exprNT :: ParserWithState Int Expr
exprNT =
    let binary = do
                    termV <- termNT
                    lift $ symbol '+'
                    exprV <- exprNT
                    x <- get
                    when (x > 3) $ lift empty
                    put (x+1)
                    return $ EBinary termV exprV
        unary = do
                    termV <- termNT
                    return $ EUnary termV
    in StateT (\s -> (runStateT binary s) <|> (runStateT unary s))

termNT :: ParserWithState Int Term
termNT =
    let binary = do
                    factorV <- factorNT
                    lift $ symbol '*'
                    termV <- termNT
                    return $ TBinary factorV termV
        unary = do
                    factorV <- factorNT
                    return $ TUnary factorV
    in  StateT (\s -> (runStateT binary s) <|> runStateT unary s)

factorNT :: ParserWithState Int Factor
factorNT =
    let left = do
                    lift $ symbol '('
                    exprV <- exprNT
                    lift $ symbol '('
                    return $ TExpr exprV
        right = do
                    numV <- lift $ number
                    return $ TNat numV
    in  StateT (\s -> (runStateT left s) <|> runStateT right s)

parseWithState :: StateT s Parser a -> s -> String -> [((a, s), String)]
parseWithState stateParser startState input =
    let parser = runStateT stateParser startState
    in parse parser input

type ParserWithState s a = StateT s Parser a








