module V2H.Parser.Sec6.AssertionStatements where
import Text.Parsec
import V2H.Ast.Sec6.AssertionStatements
import V2H.Parser.Sec9.Identifiers
-- | Incomplete production rule
assertionItemNT :: ParserSV AssertionItem
assertionItemNT = undefined