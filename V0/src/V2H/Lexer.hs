module V2H.Lexer where
import Text.Parsec
import Text.Parsec.Token

svStyle :: LanguageDef st
svStyle = LanguageDef {
    commentStart = "/*",
    commentEnd = "*/",
    commentLine = "//",
    nestedComments = True,
    identStart =  letter,
    identLetter = alphaNum <|> char '_',
    opStart = opLetter svStyle,
    opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~",
    reservedOpNames = [],
    reservedNames = [],
    caseSensitive = True
}

svLexer = makeTokenParser svStyle
svLexeme = lexeme svLexer
svIdentifier = identifier svLexer