module V2H.Parser.Sec9.Identifiers where

import V2H.Lexer
import Text.Parsec
import Data.Map
import Data.Functor.Identity

import V2H.Ast.Sec9.Identifiers

-- Identifier may change to be more complex/not lazy
portIdentifierNT = PortIdentifier <$> svIdentifier

moduleIdentifierNT :: ParserSV ModuleIdentifier
moduleIdentifierNT = ModuleIdentifier <$> svIdentifier

data IdentifierInfo =
    IdentifierInfo {
        identifierInfo :: String,
        identifierType :: IdentifierType,
        definedInFile :: FilePath
    }

data IdentifierType = Function | Task | Port
type IdentifierTable = Map String IdentifierInfo
type ParserSV = ParsecT String IdentifierTable Identity