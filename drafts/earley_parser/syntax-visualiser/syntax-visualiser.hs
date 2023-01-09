{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Control.Applicative               (Alternative (many, some, (<|>)))
import           Data.Char                         (isAlpha, isAlphaNum,
                                                    isAscii, isDigit, isSpace)
import           Data.Function
import           Data.Graph.Inductive.Example
import           Data.GraphViz
import           Data.GraphViz.Attributes.Complete
import           Data.GraphViz.Printing            hiding (list)
import           System.Directory
import           System.Environment
import           System.Environment                ()
import           System.IO
import           System.Process
import           Text.Earley                       (Grammar, Prod, fullParses,
                                                    list, parser, rule, satisfy,
                                                    token, (<?>))
import           Text.Earley.Parser.Internal
data Symbol = Node{iden::String, details::String, subSymbols::[Symbol]} | Leaf{iden::String, details::String} deriving (Show)
newtype SV = SV Symbol

isExtended x = fmap ($ x) [ isAlphaNum,
                            (==','),
                            (=='<'),
                            (=='='),
                            (=='#'),
                            (=='\"'),
                            (=='\''),
                            (=='@'),
                            (==':'),
                            (=='('),
                            (==')'),
                            (==';'),
                            (=='-'),
                            (==' '),
                            (=='_'),
                            (=='&'),
                            (=='`'),
                            (=='.'),
                            (=='>'),
                            (=='/'),
                            (=='\t'),
                            (=='['),
                            (==']'),
                            (=='{'),
                            (=='}'),
                            (=='!'),
                            (=='$'),
                            (=='*'),
                            (=='|'),
                            (=='+'),
                            (=='?')
                          ]
                & or

getSymbolLabel Node{iden, details, subSymbols} = iden ++ "-" ++ details
getSymbolLabel Leaf{iden, details}             =  iden ++ "-" ++ details

updateId :: Int -> Symbol -> (Int, Symbol)
updateId startIndex Node{iden, details, subSymbols} =
  let (maxIndex, updatedSubSymbols) = foldl (\(index, stateSymbols) symbol->
                                              let (newIndex, updatedSymbol) = updateId index symbol
                                              in  (newIndex, updatedSymbol : stateSymbols))
                                              (startIndex, []) subSymbols
  in (maxIndex + 1, Node{iden=show maxIndex, details=details, subSymbols=reverse updatedSubSymbols})
updateId startIndex Leaf{iden, details} = (startIndex+1, Leaf{iden=show startIndex, details=details})


generateGraph :: Symbol -> [(String, String, String)]
generateGraph symbol =
  case symbol of
    Node{iden, details, subSymbols} ->
      let sub_edges = map generateGraph subSymbols
          to_sub_edges = map (\subSymbol -> (getSymbolLabel symbol, getSymbolLabel subSymbol, "")) subSymbols
      in foldl (++) [] sub_edges ++ to_sub_edges
    Leaf{} -> []


grammar :: forall r. Grammar r (Prod r String Char SV)
grammar = mdo

  whitespace <- rule $ many $ satisfy isSpace

  let tok :: Prod r String Char a -> Prod r String Char a
      tok p   = whitespace *> p

      sym x   = tok $ token x <?> [x]
      idents   = tok $ (:) <$> satisfy isExtended <*> many (satisfy isExtended) <?> "identifier"
      ident   = tok $ (:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum) <?> "identifier"
      num     = tok $ some (satisfy isDigit) <?> "number"
      brackets = list " (" *> idents <* list ")"

  symbol <- rule $
                Node <$> (whitespace *> list "Node @" *> num) <*> (whitespace *> list "(tag: " *> ident <* list ") {") <*> many symbol <* whitespace <* list "}"
            <|> Leaf <$> (whitespace *> list "Leaf @" *> num) <*> brackets

  svProgram <- rule $ SV <$> (whitespace *> list "Parse Tree:" *> symbol) <* whitespace
  return svProgram

main = do
  args <- getArgs
  (_, Just out, _, _) <- createProcess (shell ("verible-verilog-syntax --printtree " ++ (args !! 0))){std_out=CreatePipe}

  treeStr <- hGetContents out
  let a = fullParses (parser grammar) treeStr
          & snd
          & unconsumed
  putStr a
  let SV parsed = (fst $ fullParses (parser grammar) treeStr) !! 0
  let graph = generateGraph $ snd $ updateId 0 $ parsed
  createDirectoryIfMissing False "syntax-visualiser/bin"
  writeFile "syntax-visualiser/bin/parse.txt" treeStr
  runGraphviz (graphElemsToDot nonClusteredParams [] graph) Png "syntax-visualiser/bin/output.png"


