module V2H.Ast.Sec1.SourceText where

import V2H.Ast.Sec1.ModuleItems
import V2H.Ast.Sec1.ConfigurationSourceText
import V2H.Ast.Sec1.PackageItems
import V2H.Ast.Sec1.ModuleParametersAndPorts
import V2H.Ast.Sec2.TypeDeclarations
import V2H.Ast.Sec5.UdpDeclaration
import V2H.Ast.Sec9.Attributes
import V2H.Ast.Sec9.Identifiers

data SourceText = SourceText {
    timeunitsDeclaration :: Maybe TimeunitsDeclaration,
    description           :: [Description]
} deriving (Show)

data Description =
    DModuleDeclaration {
        moduleDeclaration :: ModuleDeclaration
    } | DUdpDeclaration {
        udpDeclaration :: UdpDeclaration
    } | DInterfaceDeclaration {
        interfaceDeclaration :: InterfaceDeclaration
    } | DProgramDeclaration {
        programDeclaration :: ProgramDeclaration
    } | DPackageDeclaration {
        packageDeclaration :: PackageDeclaration
    } | DPackageItem {
        attributeInstances :: [AttributeInstance],
        packageItem :: PackageItem
    } | DBindDirective {
        attributeInstances :: [AttributeInstance],
        bindDirective :: BindDirective
    } | DConfigDeclaration {
        configDeclaration :: ConfigDeclaration
    } deriving (Show)

data ModuleNonAnsiHeader =
    ModuleNonAnsiHeader {
        attributeInstances :: [AttributeInstance],
        moduleKeyword :: ModuleKeyword,
        lifetime :: Maybe Lifetime,
        moduleIdentifier :: ModuleIdentifier,
        packageImportDeclarations :: [PackageImportDeclaration],
        parameterPorts :: Maybe ParameterPorts,
        ports :: [Port]
    } deriving (Show)

data ModuleAnsiHeader =
    ModuleAnsiHeader {
        attributeInstances :: [AttributeInstance],
        moduleKeyword :: ModuleKeyword,
        lifetime :: Maybe Lifetime,
        moduleIdentifier :: ModuleIdentifier,
        packageImportDeclarations :: [PackageImportDeclaration],
        parameterPorts :: Maybe ParameterPorts,
        ports :: [Port]
    } deriving (Show)

data ModuleDeclaration =
    MDNonAnsiHeader {
        moduleNonAnsiHeader :: ModuleNonAnsiHeader,
        timeunitsDeclarations :: Maybe TimeunitsDeclaration,
        moduleItems :: [ModuleItem]
    } | MDAnsiHeader {
        moduleAnsiHeader :: ModuleAnsiHeader,
        timeunitsDeclarations :: Maybe TimeunitsDeclaration,
        nonPortModuleItems :: [NonPortModuleItem]
    } | MDAttributes {
        attributeInstances :: [AttributeInstance],
        lifetime :: Maybe Lifetime,
        moduleIdentifier :: ModuleIdentifier,
        timeunitsDeclaration :: Maybe TimeunitsDeclaration,
        moduleItems :: [ModuleItem]
    } | MDNonAnsiExtern {
        moduleNonAnsiHeader :: ModuleNonAnsiHeader
    } | MDAnsiExtern {
        moduleAnsiHeader :: ModuleAnsiHeader
    } deriving (Show)

data ModuleKeyword = Module | Macromodule deriving (Show)

-- | Incomplete production rule
data InterfaceDeclaration = InterfaceDeclaration deriving (Show)

-- | Incomplete production rule
data InterfaceNonAnsiHeader = InterfaceNonAnsiHeader deriving (Show)

-- | Incomplete production rule
data InterfaceAnsiHeader = InterfaceAnsiHeader deriving (Show)

-- | Incomplete production rule
data ProgramDeclaration = ProgramDeclaration deriving (Show)

-- | Incomplete production rule
data ProgramNonAnsiHeader = ProgramNonAnsiHeader deriving (Show)

-- | Incomplete production rule
data ProgramAnsiHeader = ProgramAnsiHeader deriving (Show)

-- | Incomplete production rule
data CheckerDeclaration = CheckerDeclaration deriving (Show)

-- | Incomplete production rule
data ClassDeclaration = ClassDeclaration deriving (Show)

-- | Incomplete production rule
data InterfaceClassType = InterfaceClassType deriving (Show)

-- | Incomplete production rule
data InterfaceClassDeclaration = InterfaceClassDeclaration deriving (Show)

-- | Incomplete production rule
data InterfaceClassItem = InterfaceClassItem deriving (Show)

-- | Incomplete production rule
data InterfaceClassMethod = InterfaceClassMethod deriving (Show)

-- | Incomplete production rule
data PackageDeclaration = PackageDeclaration deriving (Show)

-- | Incomplete production rule
data TimeunitsDeclaration = TimeunitsDeclaration deriving (Show)