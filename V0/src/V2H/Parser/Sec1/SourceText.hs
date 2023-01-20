module V2H.Parser.Sec1.SourceText where


import V2H.Ast.Sec1.SourceText
import V2H.Parser.Sec1.ModuleItems
import V2H.Parser.Sec1.ConfigurationSourceText
import V2H.Parser.Sec1.PackageItems
import V2H.Parser.Sec1.ModuleParametersAndPorts
import V2H.Parser.Sec2.TypeDeclarations
import V2H.Parser.Sec5.UdpDeclaration
import V2H.Parser.Sec9.Attributes
import V2H.Parser.Sec9.Identifiers
import Text.Parsec
import Text.Parsec.String


sourceTextNT = undefined

descriptionNT = undefined

moduleNonAnsiHeaderNT = undefined

moduleAnsiHeaderNT =
    ModuleAnsiHeader <$>
        many attributeInstanceNT <*>
        moduleKeywordNT <*>
        optionMaybe lifetimeNT <*>
        moduleIdentifierNT <*>
        many packageImportDeclarationNT <*>
        optionMaybe parameterPortsNT <*>
        many portNT

-- | Incomplete production rule
moduleDeclarationNT :: Parsec String u ModuleDeclaration
moduleDeclarationNT =
    MDAnsiHeader <$> moduleAnsiHeaderNT <*> (optionMaybe timeunitsDeclarationNT) <*> many nonPortModuleItemNT

moduleKeywordNT = undefined

-- | Incomplete production rule
interfaceDeclarationNT = undefined

-- | Incomplete production rule
interfaceNonAnsiHeaderNT = undefined

-- | Incomplete production rule
interfaceAnsiHeaderNT = undefined

-- | Incomplete production rule
programDeclarationNT = undefined

-- | Incomplete production rule
programNonAnsiHeaderNT = undefined

-- | Incomplete production rule
programAnsiHeaderNT = undefined

-- | Incomplete production rule
checkerDeclarationNT = undefined

-- | Incomplete production rule
classDeclarationNT = undefined

-- | Incomplete production rule
interfaceClassTypeNT = undefined

-- | Incomplete production rule
interfaceClassDeclarationNT = undefined

-- | Incomplete production rule
interfaceClassItemNT = undefined

-- | Incomplete production rule
interfaceClassMethodNT = undefined

-- | Incomplete production rule
packageDeclarationNT = undefined

-- | Incomplete production rule
timeunitsDeclarationNT = undefined