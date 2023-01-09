{-# LANGUAGE RecursiveDo #-}
import           Control.Applicative
import           Data.Char
import           System.Environment

import           Text.Earley

grammar :: Grammar r (Prod r String Char [String])
grammar = mdo
  whitespace  <- rule $ () <$ many (satisfy isSpace)
  whitespace1 <- rule $ () <$ some (satisfy isSpace) <?> "whitespace"


  ident <- rule
    $ (:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)
   <?> "identifier"

  expr <- rule
    $  (:)   <$> ident <* whitespace1 <*> expr
   <|> (:[]) <$> ident <* whitespace

  return expr

main :: IO ()
main = do
  x <- getLine
  print $ fullParses (parser grammar) x
