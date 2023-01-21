module V2H.Parser.Sec6.ProceduralBlocksAndAssignments where

import V2H.Ast.Sec6.ProceduralBlocksAndAssignments
import V2H.Parser.Sec6.Statements
import V2H.Lexer
import Text.Parsec
import Text.Parsec.Char

-- | Incomplete production rule
initialConstructNT = undefined

-- | Incomplete production rule
alwaysConstructNT = AlwaysConstruct <$> alwaysKeywordNT <*> statementNT

-- | Incomplete production rule
alwaysKeywordNT = alwaysFFNT <|> alwaysLatchNT <|> alwaysCombNT <|> alwaysNT
alwaysFFNT = svLexeme (string "alwaysFF") *> pure AlwaysFF
alwaysLatchNT = svLexeme (string "always_latch") *> pure AlwaysLatch
alwaysCombNT = svLexeme (string "always_comb") *> pure AlwaysComb
alwaysNT = svLexeme (string "always") *> pure Always

-- | Incomplete production rule
finalConstructNT = undefined