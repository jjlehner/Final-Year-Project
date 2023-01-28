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

import V2H.Lexer

sourceTextNT :: ParserSV SourceText
sourceTextNT = SourceText <$> optionMaybe timeunitsDeclarationNT <*> many descriptionNT

descriptionNT :: ParserSV Description
descriptionNT = DModuleDeclaration <$> moduleDeclarationNT
                -- <|> DUdpDeclaration <$> udpDeclarationNT
                -- <|> DInterfaceDeclaration <$> interfaceDeclarationNT
                -- <|> DProgramDeclaration <$> programDeclarationNT
                -- <|> DPackageDeclaration <$> packageDeclarationNT
                -- <|> DPackageItem <$> many attributeInstanceNT <*> packageItemNT
                -- <|> DBindDirective <$> many attributeInstanceNT <*> bindDirectiveNT
                -- <|> DConfigDeclaration <$> configDeclarationNT

moduleNonAnsiHeaderNT :: ParserSV ModuleNonAnsiHeader
moduleNonAnsiHeaderNT = undefined


moduleAnsiHeaderNT :: ParserSV ModuleAnsiHeader
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
moduleDeclarationNT :: ParserSV ModuleDeclaration
moduleDeclarationNT =
    MDAnsiHeader <$> moduleAnsiHeaderNT <*> (optionMaybe timeunitsDeclarationNT) <*> many nonPortModuleItemNT

moduleKeywordNT :: ParserSV ModuleKeyword
moduleKeywordNT =   svLexeme (string "module") *> pure Module
                    <|> svLexeme (string "macromodule") *> pure MacroModule

-- | Incomplete production rule
interfaceDeclarationNT :: ParserSV InterfaceDeclaration
interfaceDeclarationNT = undefined

-- | Incomplete production rule
interfaceNonAnsiHeaderNT :: ParserSV InterfaceNonAnsiHeader
interfaceNonAnsiHeaderNT = undefined

-- | Incomplete production rule
interfaceAnsiHeaderNT :: ParserSV InterfaceAnsiHeader
interfaceAnsiHeaderNT = undefined

-- | Incomplete production rule
programDeclarationNT :: ParserSV ProgramDeclaration
programDeclarationNT = undefined

-- | Incomplete production rule
programNonAnsiHeaderNT :: ParserSV ProgramNonAnsiHeader
programNonAnsiHeaderNT = undefined

-- | Incomplete production rule
programAnsiHeaderNT :: ParserSV ProgramAnsiHeader
programAnsiHeaderNT = undefined

-- | Incomplete production rule
checkerDeclarationNT :: ParserSV CheckerDeclaration
checkerDeclarationNT = undefined

-- | Incomplete production rule
classDeclarationNT :: ParserSV ClassDeclaration
classDeclarationNT = undefined

-- | Incomplete production rule
interfaceClassTypeNT :: ParserSV InterfaceClassType
interfaceClassTypeNT = undefined

-- | Incomplete production rule
interfaceClassDeclarationNT :: ParserSV InterfaceClassDeclaration
interfaceClassDeclarationNT = undefined

-- | Incomplete production rule
interfaceClassItemNT :: ParserSV InterfaceClassItem
interfaceClassItemNT = undefined

-- | Incomplete production rule
interfaceClassMethodNT :: ParserSV InterfaceClassMethod
interfaceClassMethodNT = undefined

-- | Incomplete production rule
packageDeclarationNT :: ParserSV PackageDeclaration
packageDeclarationNT = undefined

-- | Incomplete production rule
timeunitsDeclarationNT :: ParserSV TimeunitsDeclaration
timeunitsDeclarationNT = return TimeunitsDeclaration