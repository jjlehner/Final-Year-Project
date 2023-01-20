module V2H.Parser (
    module V2H.Parser.Sec1.ConfigurationSourceText,
    module V2H.Parser.Sec1.LibrarySourceText,
    module V2H.Parser.Sec1.ModuleItems,
    module V2H.Parser.Sec1.ModuleParametersAndPorts,
    module V2H.Parser.Sec1.PackageItems,
    module V2H.Parser.Sec1.SourceText,
    module V2H.Parser.Sec2.DeclarationAssignments,
    module V2H.Parser.Sec2.TypeDeclarations,
    module V2H.Parser.Sec4.GeneratedInstantiation,
    module V2H.Parser.Sec4.InterfaceInstantiation,
    module V2H.Parser.Sec4.ProgramInstantiation,
    module V2H.Parser.Sec5.UdpDeclaration,
    module V2H.Parser.Sec6.AssertionStatements,
    module V2H.Parser.Sec6.ContinuousAssignmentAndNetAliasStatements,
    module V2H.Parser.Sec6.ProceduralBlocksAndAssignments,
    module V2H.Parser.Sec8.Primaries,
    module V2H.Parser.Sec9.Attributes,
    module V2H.Parser.Sec9.Identifiers
) where

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
import Data.Map (Map)

data IdentifierInfo =
    IdentifierInfo {
        identifierInfo :: String,
        identifierType :: IdentifierType,
        definedInFile :: FilePath
    }

data IdentifierType = Function | Task
type IdentifierTable = Map String IdentifierInfo

-- parseSource :: Text -> AstRoot
-- parseSource =

