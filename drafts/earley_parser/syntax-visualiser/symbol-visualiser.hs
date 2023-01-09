module Main where

import           Control.Monad
import           Control.Monad.State.Lazy
import           Data.Function
import           Data.GraphViz
import           Data.List
import           Debug.Trace
import           System.Environment

import           Text.Parsec              (between, char, digit, eof, letter,
                                           many, many1, oneOf, optional, runP,
                                           sepBy, string, try, (<|>))
import           Text.Parsec.Char
import           Text.Parsec.String
bnfFiles = [
    "syntax-visualiser/bnf/sec-1/1.1-library-source-text.txt",
    "syntax-visualiser/bnf/sec-1/1.2-SystemVerilog-source-text.txt",
    "syntax-visualiser/bnf/sec-1/1.3-module-parameters-and-ports.txt",
    "syntax-visualiser/bnf/sec-1/1.4-module-items.txt",
    "syntax-visualiser/bnf/sec-1/1.5-configuration-source-text.txt",
    "syntax-visualiser/bnf/sec-1/1.6-interface-items.txt",
    "syntax-visualiser/bnf/sec-1/1.7-program-items.txt",
    "syntax-visualiser/bnf/sec-1/1.8-checker-items.txt",
    "syntax-visualiser/bnf/sec-1/1.9-class-items.txt",
    "syntax-visualiser/bnf/sec-1/1.10-constraints.txt",
    "syntax-visualiser/bnf/sec-1/1.11-package-items.txt"
    ]

type SymbolIdentifier = String
type SymbolExpression = [Symbol]
data Symbol = SingleSymbol SymbolIdentifier | OptionalSymbol [SymbolExpression] | MultipleSymbol [SymbolExpression] deriving (Show, Eq)
data NonTerminal = NonTerminal {name::SymbolIdentifier, symbols::[SymbolExpression]} deriving (Show, Eq)

readGrammarFiles = do
    grammars <- traverse readFile bnfFiles
    return $ intercalate "\n" grammars

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \t"
lexeme p = p <* whitespace
symbol c = void $ lexeme $ char c
identifier = lexeme $ many1 idenChars
  where
    idenChars = letter
                <|> char '_'
                <|> char ';'
                <|> char ','
                <|> char '-'
                <|> char ':'
                <|> char '('
                <|> char ')'
                <|> char '.'
                <|> char '*'
                <|> char '/'
                <|> char '#'
                <|> char '='
                <|> char '$'
                <|> char '>'
                <|> digit

symbolP :: Parser Symbol
symbolP =       SingleSymbol <$> identifier
            <|> OptionalSymbol <$> between (symbol '{') (symbol  '}') (sepBy exprP expressionSeparatorP)
            <|>  MultipleSymbol <$> between (symbol '[') (symbol ']') (sepBy exprP expressionSeparatorP)

exprP ::Parser [Symbol]
exprP = many1 symbolP

expressionSeparatorP :: Parser ()
expressionSeparatorP = (try $ optional (char '\n') *> symbol '|')

nonTerminalProd :: Parser NonTerminal
nonTerminalProd = NonTerminal <$> identifier <* lexeme (string "::=") <*> sepBy exprP expressionSeparatorP

grammarFileProd :: Parser [NonTerminal]
grammarFileProd = sepBy nonTerminalProd (symbol '\n') <* eof

parseGrammarFiles grammarString =
    case (runP grammarFileProd () "Grammar File" grammarString) of
        Left parseError   -> error $ show parseError
        Right nonTerimals -> nonTerimals

symbolIdentifierInSymbol :: SymbolIdentifier -> Symbol -> Bool
symbolIdentifierInSymbol symbolIdentifier symbol =
    case symbol of
       SingleSymbol iden -> iden == symbolIdentifier
       OptionalSymbol optional -> any (symbolIdentifierInSymbolExpression symbolIdentifier) optional
       MultipleSymbol multiple -> any (symbolIdentifierInSymbolExpression symbolIdentifier)  multiple

symbolIdentifierInSymbolExpression :: SymbolIdentifier -> [Symbol] -> Bool
symbolIdentifierInSymbolExpression symbolIdentifier =
    any (symbolIdentifierInSymbol symbolIdentifier)

symbolIdentifierInNonTerminal :: SymbolIdentifier -> NonTerminal -> Bool
symbolIdentifierInNonTerminal symbolIdentifier (NonTerminal {name, symbols}) = any (symbolIdentifierInSymbolExpression symbolIdentifier) symbols

type UnExploredNonTerminal = NonTerminal
type ExploredNonTerminal = NonTerminal
type GraphEdge = (SymbolIdentifier, SymbolIdentifier, ())

findSymbolIdentifierInNonTerminal :: [NonTerminal] -> [ExploredNonTerminal] -> SymbolIdentifier -> ([UnExploredNonTerminal], [ExploredNonTerminal])
findSymbolIdentifierInNonTerminal allNonTerminals exploredNonTerminals symbolIdentifier =
    (unExploredParents, exploredParents) where
    parents = filter (symbolIdentifierInNonTerminal symbolIdentifier) allNonTerminals
    unExploredParents = parents \\ exploredNonTerminals
    exploredParents = parents `intersect` exploredNonTerminals

makeEdges :: SymbolIdentifier -> [NonTerminal] -> [GraphEdge]
makeEdges startingSymbol parents =
    map (\parent -> (name parent, startingSymbol, ())) parents

parentEdgesFolder :: [NonTerminal] -> ([GraphEdge], [NonTerminal]) -> SymbolIdentifier -> ([GraphEdge], [ExploredNonTerminal])
parentEdgesFolder allNonTerminals (edges, exploredNonTerminals) symbolIdentifier =
    let (newTerminals, newExplored) = findSymbolIdentifierInNonTerminal allNonTerminals exploredNonTerminals symbolIdentifier
        (parentEdges, finalExplored) =  map (name) newTerminals
                                        & findAllGrammarTreeEdges allNonTerminals (newTerminals ++ newExplored ++ exploredNonTerminals)
    in  (makeEdges symbolIdentifier newTerminals
        & (++) (makeEdges symbolIdentifier newExplored)
        & (++) parentEdges
        & (++) edges
        , exploredNonTerminals ++ newExplored ++ finalExplored)

findAllGrammarTreeEdges :: [NonTerminal] -> [NonTerminal] -> [SymbolIdentifier] -> ([GraphEdge], [ExploredNonTerminal])
findAllGrammarTreeEdges allNonTerminals exploredNonTerminals startingSymbols =
    foldl (parentEdgesFolder allNonTerminals) ([], exploredNonTerminals) startingSymbols

drawGrammarTree :: [NonTerminal] -> SymbolIdentifier -> DotGraph String
drawGrammarTree allNonTerminals startingSymbolIdentifier =
    findAllGrammarTreeEdges allNonTerminals [] [startingSymbolIdentifier]
    & fst
    & graphElemsToDot nonClusteredParams []

main = do
    args <- getArgs
    grammar <- readGrammarFiles
    let nonTerminals = parseGrammarFiles grammar
    runGraphviz (drawGrammarTree nonTerminals "timeunits_declaration") Png "syntax-visualiser/bin/output.png"
