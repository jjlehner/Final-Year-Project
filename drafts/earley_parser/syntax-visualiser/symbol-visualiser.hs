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
bnfFilesSec1 = [
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
bnfFilesSec2 = [
    "syntax-visualiser/bnf/sec-2/2.1.1-module-parameter-declarations.txt",
    "syntax-visualiser/bnf/sec-2/2.3-declarations-lists.txt",
    "syntax-visualiser/bnf/sec-2/2.1.2-port-declarations.txt",
    "syntax-visualiser/bnf/sec-2/2.4-declaration-assignments.txt",
    "syntax-visualiser/bnf/sec-2/2.1.3-type-declarations.txt",
    "syntax-visualiser/bnf/sec-2/2.5-declaration-ranges.txt",
    "syntax-visualiser/bnf/sec-2/2.10-assertion-declarations.txt",
    "syntax-visualiser/bnf/sec-2/2.6-function-declarations.txt",
    "syntax-visualiser/bnf/sec-2/2.11-covergroup-declarations.txt",
    "syntax-visualiser/bnf/sec-2/2.7-task-declarations.txt",
    "syntax-visualiser/bnf/sec-2/2.2.1-net-and-variable-types.txt",
    "syntax-visualiser/bnf/sec-2/2.8-block-item-declarations.txt",
    "syntax-visualiser/bnf/sec-2/2.2.2-strengths.txt",
    "syntax-visualiser/bnf/sec-2/2.9-interface-declarations.txt",
    "syntax-visualiser/bnf/sec-2/2.2.3-delays.txt"
    ]

bnfFilesSec3 = [
    "syntax-visualiser/bnf/sec-3/3.1-primitive-instantiation-and-instances.txt",
    "syntax-visualiser/bnf/sec-3/3.2-primitive-strengths.txt",
    "syntax-visualiser/bnf/sec-3/3.3-primitive-terminals.txt",
    "syntax-visualiser/bnf/sec-3/3.4-primitive-gate-and-switch-types.txt"
    ]

bnfFilesSec4 = [
    "syntax-visualiser/bnf/sec-4/4.1.1-module-instantiation.txt",
    "syntax-visualiser/bnf/sec-4/4.1.2-interface-instantiation.txt",
    "syntax-visualiser/bnf/sec-4/4.1.3-program-instantiation.txt",
    "syntax-visualiser/bnf/sec-4/4.1.4-checker-instantiation.txt",
    "syntax-visualiser/bnf/sec-4/4.2-generated-instantiation.txt"
    ]

bnfFilesSec5 = [
    "syntax-visualiser/bnf/sec-5/5.1-udp-declaration.txt",
    "syntax-visualiser/bnf/sec-5/5.2-udp-ports.txt",
    "syntax-visualiser/bnf/sec-5/5.3-udp-body.txt",
    "syntax-visualiser/bnf/sec-5/5.4-udp-instantiation.txt"
    ]

bnfFilesSec6 = [
    "syntax-visualiser/bnf/sec-6/6.1-continous-assignment-and-net-alias-statements.txt",
    "syntax-visualiser/bnf/sec-6/6.10-assertion-statements.txt",
    "syntax-visualiser/bnf/sec-6/6.11-clocking-block.txt",
    "syntax-visualiser/bnf/sec-6/6.12-randsequence.txt",
    "syntax-visualiser/bnf/sec-6/6.2-procedural-blocks-and-assignments.txt",
    "syntax-visualiser/bnf/sec-6/6.3-parallel-and-sequential-blocks.txt",
    "syntax-visualiser/bnf/sec-6/6.4-statements.txt",
    "syntax-visualiser/bnf/sec-6/6.5-timing-control-statements.txt",
    "syntax-visualiser/bnf/sec-6/6.6-conditional-statements.txt",
    "syntax-visualiser/bnf/sec-6/6.7-case-statements.txt",
    "syntax-visualiser/bnf/sec-6/6.7.1-patterns.txt",
    "syntax-visualiser/bnf/sec-6/6.8-looping-statements.txt",
    "syntax-visualiser/bnf/sec-6/6.9-subroutine-call-statements.txt"
    ]

bnfFilesSec7 = [
    "syntax-visualiser/bnf/sec-7/7.1-specify-block-declaration.txt",
    "syntax-visualiser/bnf/sec-7/7.2-specify-path-declarations.txt",
    "syntax-visualiser/bnf/sec-7/7.3-specify-block-terminals.txt",
    "syntax-visualiser/bnf/sec-7/7.4-specify-path-delays.txt",
    "syntax-visualiser/bnf/sec-7/7.5.1-system-timing-check-commands.txt",
    "syntax-visualiser/bnf/sec-7/7.5.2-system-timing-check-command-arguments.txt",
    "syntax-visualiser/bnf/sec-7/7.5.3-system-timing-check-event-definitions.txt"
    ]

bnfFilesSec8 = [
    "syntax-visualiser/bnf/sec-8/8.1-concatenations.txt",
    "syntax-visualiser/bnf/sec-8/8.2-subroutine-calls.txt",
    "syntax-visualiser/bnf/sec-8/8.3-expressions.txt",
    "syntax-visualiser/bnf/sec-8/8.4-primaries.txt",
    "syntax-visualiser/bnf/sec-8/8.5-expression-left-side-values.txt",
    "syntax-visualiser/bnf/sec-8/8.6-operators.txt",
    "syntax-visualiser/bnf/sec-8/8.7-numbers.txt",
    "syntax-visualiser/bnf/sec-8/8.8-strings.txt"
    ]

bnfFilesSec9 = [
    "syntax-visualiser/bnf/sec-9/9.1-attributes.txt",
    "syntax-visualiser/bnf/sec-9/9.2-comments.txt",
    "syntax-visualiser/bnf/sec-9/9.3-identifiers.txt",
    "syntax-visualiser/bnf/sec-9/9.4-white-space.txt"
    ]

bnfFiles = bnfFilesSec1
            ++ bnfFilesSec2
            ++ bnfFilesSec3
            ++ bnfFilesSec4
            ++ bnfFilesSec5
            ++ bnfFilesSec6
            ++ bnfFilesSec7
            ++ bnfFilesSec8
            ++ bnfFilesSec9

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
stringSymbol str = void $ lexeme $ string str

escaped c = do
    try $ string $ "\\" ++ [c]
    return c

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
                <|> char '<'
                <|> char '+'
                <|> char '"'
                <|> char '@'
                <|> escaped '\n'
                <|> escaped '\t'
                <|> escaped ']'
                <|> escaped '['
                <|> char '!'
                <|> char '&'
                <|> escaped '|'
                <|> escaped '{'
                <|> escaped '}'
                <|> escaped '\\'
                <|> char '%'
                <|> char '\''
                <|> char '?'
                <|> char '^'
                <|> char '~'
                <|> digit

symbolP :: Parser Symbol
symbolP =       SingleSymbol <$> identifier
            <|> OptionalSymbol <$> between (symbol '[') (symbol  ']') (sepBy exprP expressionSeparatorP)
            <|>  MultipleSymbol <$> between (symbol '{') (symbol '}') (sepBy exprP expressionSeparatorP)

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
    trace (show $ length exploredNonTerminals) foldl (parentEdgesFolder allNonTerminals) ([], exploredNonTerminals) startingSymbols

drawGrammarTree :: [NonTerminal] -> SymbolIdentifier -> DotGraph String
drawGrammarTree allNonTerminals startingSymbolIdentifier =
    findAllGrammarTreeEdges allNonTerminals [] [startingSymbolIdentifier]
    & fst
    & graphElemsToDot nonClusteredParams []

main = do
    args <- getArgs
    grammar <- readGrammarFiles
    let nonTerminals = parseGrammarFiles grammar
    runGraphviz (drawGrammarTree nonTerminals "eof") Png "syntax-visualiser/bin/output.png"
