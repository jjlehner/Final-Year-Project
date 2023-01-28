module Main where
import V2H.Parser
import Data.Functor.Identity
main = do
    s <- readFile "tests/parser/simple/empty_module.sv"
    let top = parseSource "simple_module" s
    case runIdentity top of
        Left left -> print left
        Right right -> print right