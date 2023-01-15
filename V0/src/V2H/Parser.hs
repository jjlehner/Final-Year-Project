module V2H.Parser where

import V2H.Ast
import Data.Map (Map)

data IdentifierInfo =
    IdentifierInfo {
        identifierInfo :: String,
        identifierType :: IdentifierType,
        definedInFile :: FilePath
    }

type IdentifierTable = Map String IdentifierInfo

parseSource :: Text -> AstRoot
parseSource =

