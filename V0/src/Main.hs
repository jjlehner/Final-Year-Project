module Main where
import Prelude hiding (readFile)
import Data.ByteString.Lazy.Char8 (readFile)
import V2H.Alex.Lexer as Lexer
import V2H.Parser as Parser
import V2H.Component as Component

main = do
    s <- readFile "tests/parser/simple/empty_module.sv"
    let x = do {
                tokens <- Lexer.runLexer s;
                let ast = Parser.runParser tokens;
                in return $ Component.toComponents $ head (fst ast)
            }
    print x

    return ()

