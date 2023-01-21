module V2H.Parser.Sec9.Identifiers where
import Text.Parsec
import Data.Map
import Data.Functor.Identity
-- Identifier may change to be more complex/not lazy
portIdentifierNT = undefined
moduleIdentifierNT = undefined

data IdentifierInfo =
    IdentifierInfo {
        identifierInfo :: String,
        identifierType :: IdentifierType,
        definedInFile :: FilePath
    }

data IdentifierType = Function | Task | Port
type IdentifierTable = Map String IdentifierInfo
type ParserSV = ParsecT String IdentifierTable Identity