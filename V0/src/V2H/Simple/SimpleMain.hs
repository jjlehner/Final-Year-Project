module Main where

import Prelude hiding (readFile)

import Data.ByteString.Lazy.Char8 (readFile)

import V2H.Alex.Lexer qualified as Lexer
import V2H.Simple.Parser qualified as Parser
import V2H.Simple.IRGenerator as IR

import Text.Pretty.Simple
main = do
    sourceCode <- readFile "tests/parser/simple/empty_module.sv"
    let ir =
            Lexer.runLexer sourceCode
            >>= Parser.runParser
            >>= IR.generateIR
    pPrint ir
