module Main where
import Prelude hiding (readFile)
import Data.Functor.Identity
import Data.ByteString.Lazy.Char8 (readFile)
import V2H.Alex.Lexer
import V2H.Happy.Parser


main = do
    s <- readFile "tests/parser/simple/empty_module.sv"
    print $ scanMany s
    print $ runAlex s parseSV


