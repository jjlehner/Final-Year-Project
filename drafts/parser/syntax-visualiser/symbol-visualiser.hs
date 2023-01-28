module Main where

import           Control.Monad
import           Control.Monad.State.Lazy
import           Data.Function
import           Data.GraphViz
import           Data.List
import           Data.Maybe
import           Data.Set
import           System.Environment

import           Data.GraphViz.Attributes.Complete
import           Text.Parsec                       (between, char, digit, eof,
                                                    letter, many, many1, oneOf,
                                                    optional, runP, sepBy,
                                                    string, try, (<|>))
import           Text.Parsec.Char
import           Text.Parsec.String

import           System.Console.CmdArgs            (Data, Typeable, cmdArgs,
                                                    def, details, help, opt,
                                                    summary, typ, verbosity,
                                                    versionArg, (&=))
import           System.Console.CmdArgs.Implicit   (argPos)

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
data Symbol = SingleSymbol SymbolIdentifier | OptionalSymbol [SymbolExpression] | MultipleSymbol [SymbolExpression] deriving (Show, Eq, Ord)
data NonTerminal = NonTerminal {name::SymbolIdentifier, symbols::[SymbolExpression]} deriving (Show, Eq, Ord)

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

findSymbolIdentifierInNonTerminal :: Set NonTerminal -> Set ExploredNonTerminal -> SymbolIdentifier -> (Set UnExploredNonTerminal, Set ExploredNonTerminal)
findSymbolIdentifierInNonTerminal allNonTerminals exploredNonTerminals symbolIdentifier =
    (unExploredParents, exploredParents) where
    parents = Data.Set.filter (symbolIdentifierInNonTerminal symbolIdentifier) allNonTerminals
    unExploredParents = parents Data.Set.\\ exploredNonTerminals
    exploredParents = parents `intersection` exploredNonTerminals

makeEdges :: SymbolIdentifier -> Set NonTerminal -> Set GraphEdge
makeEdges startingSymbol parents =
    Data.Set.map (\parent -> (name parent, startingSymbol, ())) parents

parentEdgesFolder :: Set NonTerminal -> (Set GraphEdge, Set NonTerminal) -> SymbolIdentifier -> (Set GraphEdge, Set ExploredNonTerminal)
parentEdgesFolder allNonTerminals (edges, exploredNonTerminals) symbolIdentifier =
    let (newTerminals, newExplored) = findSymbolIdentifierInNonTerminal allNonTerminals exploredNonTerminals symbolIdentifier
        (parentEdges, finalExplored) =  Data.Set.map (name) newTerminals
                                        & findAllGrammarTreeEdges allNonTerminals (newTerminals `Data.Set.union` newExplored `Data.Set.union` exploredNonTerminals)
    in  (makeEdges symbolIdentifier newTerminals
        & Data.Set.union (makeEdges symbolIdentifier newExplored)
        & Data.Set.union parentEdges
        & Data.Set.union edges
        , exploredNonTerminals `Data.Set.union` newExplored `Data.Set.union` finalExplored)

findAllGrammarTreeEdges :: Set NonTerminal -> Set NonTerminal -> Set SymbolIdentifier -> (Set GraphEdge, Set ExploredNonTerminal)
findAllGrammarTreeEdges allNonTerminals exploredNonTerminals startingSymbols =
    Data.Set.foldl (parentEdgesFolder allNonTerminals) (Data.Set.empty, exploredNonTerminals) startingSymbols

runPathFinding :: SymbolIdentifier -> Maybe SymbolIdentifier -> [GraphEdge] -> Maybe [GraphEdge]
runPathFinding start (Just root) graphEdges =
    findPath graphEdges root start Data.Set.empty
runPathFinding _ Nothing graphEdges = Just graphEdges

drawGrammarTree :: Set NonTerminal -> SymbolIdentifier -> Maybe SymbolIdentifier -> DotGraph String
drawGrammarTree allNonTerminals startingSymbolIdentifier maybeRoot =
    findAllGrammarTreeEdges allNonTerminals Data.Set.empty (Data.Set.singleton startingSymbolIdentifier)
    & fst
    & toList
    & runPathFinding startingSymbolIdentifier maybeRoot
    & fromJust
    & Data.Set.toList . Data.Set.fromList
    & graphElemsToDot params []
    where
        edgeAttribute color penWidth = [ Color $ toColorList [ color ], PenWidth $ penWidth]
        params =  nonClusteredParams {
                        fmtEdge = \(from, to, el) ->
                            if to == startingSymbolIdentifier then
                                edgeAttribute (RGB 0xff 0 0) 10
                            else
                                edgeAttribute ( RGB 0 0 0 ) 1
        }

traverseTuple :: (a, Maybe [a]) -> Maybe [a]
traverseTuple (_, Nothing) = Nothing
traverseTuple (a, Just b)  = Just $ a:b

findPath :: [GraphEdge] -> SymbolIdentifier -> SymbolIdentifier -> Set SymbolIdentifier -> Maybe [GraphEdge]
findPath graphEdges start end avoid =
    let subEdges = Data.List.filter (\(from, _, _) -> start == from) graphEdges
        immediateSubTerminals = subEdges
                                & Data.List.map (\(_, to, _) -> to)
        avoid' = Data.Set.insert start avoid
    in
        if start == end then
            Just []
        else if member start avoid || member end avoid then
            Nothing
        else
            Data.List.map (\subTerminal -> findPath graphEdges subTerminal end avoid') immediateSubTerminals
            & zip subEdges
            & Data.List.map traverseTuple
            & catMaybes
            & concat
            & \x -> case x of
                        [] -> Nothing
                        _  -> Just x

data SymbolVisualiser = SymbolVisualiser
    {
        startingSymbol :: SymbolIdentifier,
        avoid          :: [SymbolIdentifier],
        root           :: Maybe SymbolIdentifier,
        printTerminals :: Bool
    }
    deriving (Show,Eq, Data, Typeable)


args = SymbolVisualiser
    {   startingSymbol = def &= argPos 0,
        avoid = [] &= typ "SYMBOL_IDENTIFIER" &= help "Non-Terminals to avoid",
        root = Nothing &= typ "SYMBOL_IDENTIFIER" &= help "Root NonTerminal",
        printTerminals = False &= typ "Bool" &= help "Print terminals"
   } &= versionArg [summary ""]


getAllSymbolIdentifiersFromSymbol :: Symbol -> Set SymbolIdentifier
getAllSymbolIdentifiersFromSymbol symbol =
   case symbol of
        SingleSymbol identifier -> Data.Set.singleton identifier
        OptionalSymbol expr -> Data.List.map getAllSymbolIdentifiersFromExpression expr
                               & mconcat
        MultipleSymbol expr -> Data.List.map getAllSymbolIdentifiersFromExpression expr
                               & mconcat

getAllSymbolIdentifiersFromExpression :: SymbolExpression -> Set SymbolIdentifier
getAllSymbolIdentifiersFromExpression expression =
    Data.List.map getAllSymbolIdentifiersFromSymbol expression
    & mconcat

getAllSymbolIdentifiersFromExpressionList :: [SymbolExpression] -> Set SymbolIdentifier
getAllSymbolIdentifiersFromExpressionList expressionList =
    Data.List.map getAllSymbolIdentifiersFromExpression expressionList
    & mconcat

getTerminals :: Set NonTerminal -> [SymbolIdentifier]
getTerminals g =
    let symbolIdentifiers = Data.Set.map (getAllSymbolIdentifiersFromExpressionList . symbols) g
                            & unions
        nonTerminals = Data.List.foldl (\s nt -> name nt : s) [] g
                       & fromList
    in Data.Set.difference symbolIdentifiers nonTerminals
       & toList

main = do
    a <- cmdArgs args
    grammar <- readGrammarFiles
    let nonTerminals = parseGrammarFiles grammar
                        & Data.Set.fromList
                        & Data.Set.filter (\x -> not $ member (name x) $ fromList $ avoid a)
    when (printTerminals a)
        $ writeFile "syntax-visualiser/bin/terminals.txt"
        $ intercalate "\n"
        $ Data.List.map show (getTerminals nonTerminals)
    runGraphviz (drawGrammarTree nonTerminals (startingSymbol a) (root a)) Png "syntax-visualiser/bin/output.png"
