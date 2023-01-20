module V2H.Parser.Sec6.ProceduralBlocksAndAssignments where

import V2H.Ast.Sec6.ProceduralBlocksAndAssignments
import Text.Parsec
import Text.Parsec.Char

-- | Incomplete production rule
initialConstructNT = undefined

-- | Incomplete production rule
alwaysConstructNT = AlwaysConstruct <$> alwaysKeywordNT <*> statementNT

-- | Incomplete production rule
alwaysKeywordNT = string' "always_ff" <|> string' "always_latch" <|> string' "always_comb" <|> string' "always"
alwaysFFNT = do
    string' "always_ff"
    return AlwaysFF

-- | Incomplete production rule
finalConstructNT = undefined