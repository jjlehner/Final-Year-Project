module V2H.Parser.Sec8.Primaries where

import V2H.Ast.Sec8.Primaries
import V2H.Parser.Sec9.Identifiers
-- | Incomplete production rule
constantSelectNT :: ParserSV ConstantSelect
constantSelectNT = return ConstantSelect{}