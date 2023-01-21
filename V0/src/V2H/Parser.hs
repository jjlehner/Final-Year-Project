module V2H.Parser where
import V2H.Ast

import V2H.Parser.Sec1.ConfigurationSourceText
import V2H.Parser.Sec1.LibrarySourceText
import V2H.Parser.Sec1.ModuleItems
import V2H.Parser.Sec1.ModuleParametersAndPorts
import V2H.Parser.Sec1.PackageItems
import V2H.Parser.Sec1.SourceText
import V2H.Parser.Sec2.DeclarationAssignments
import V2H.Parser.Sec2.TypeDeclarations
import V2H.Parser.Sec4.GeneratedInstantiation
import V2H.Parser.Sec4.InterfaceInstantiation
import V2H.Parser.Sec4.ProgramInstantiation
import V2H.Parser.Sec5.UdpDeclaration
import V2H.Parser.Sec6.AssertionStatements
import V2H.Parser.Sec6.ContinuousAssignmentAndNetAliasStatements
import V2H.Parser.Sec6.ProceduralBlocksAndAssignments
import V2H.Parser.Sec8.Primaries
import V2H.Parser.Sec9.Attributes
import V2H.Parser.Sec9.Identifiers
import Data.Map

import Text.Parsec
data IdentifierInfo =
    IdentifierInfo {
        identifierInfo :: String,
        identifierType :: IdentifierType,
        definedInFile :: FilePath
    }

data IdentifierType = Function | Task
type IdentifierTable = Map String IdentifierInfo

parseSource :: SourceName -> String -> Either ParseError AstRoot
parseSource sourceName source = runParser astRootNT empty sourceName source

astRootNT = sourceTextNT <|> libraryTextNT

