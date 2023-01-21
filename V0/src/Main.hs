module Main where
import V2H.Parser
main = do
    s <- readFile "tests/parser/simple/empty_module.sv"
    let top = parseSource "simple_module" s
    case top of
        Left left -> putStr "Error"
        Right _ -> putStr "Success"