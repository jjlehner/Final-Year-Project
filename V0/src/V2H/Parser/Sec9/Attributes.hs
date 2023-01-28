module V2H.Parser.Sec9.Attributes where
import V2H.Ast.Sec9.Attributes
import V2H.Parser.Sec9.Identifiers
import Text.Parsec

-- | Incomplete production rule
attributeInstanceNT :: ParserSV AttributeInstance
attributeInstanceNT = try $ string "(* *)" *> return AttributeInstance