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
}

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
    }

data ModuleNonAnsiHeader =
    ModuleNonAnsiHeader {
        attributeInstances :: [AttributeInstance],
        moduleKeyword :: ModuleKeyword,
        lifetime :: Maybe Lifetime,
        moduleIdentifier :: ModuleIdentifier,
        packageImportDeclarations :: [PackageImportDeclaration],
        parameterPorts :: Maybe ParameterPorts,
        ports :: [Port]
    }

data ModuleAnsiHeader =
    ModuleAnsiHeader {
        attributeInstances :: [AttributeInstance],
        moduleKeyword :: ModuleKeyword,
        lifetime :: Maybe Lifetime,
        moduleIdentifier :: ModuleIdentifier,
        packageImportDeclarations :: [PackageImportDeclaration],
        parameterPorts :: Maybe ParameterPorts,
        ports :: [Port]
    }

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
    }

data ModuleKeyword = Module | MacroModule

-- | Incomplete production rule
data InterfaceDeclaration = InterfaceDeclaration

-- | Incomplete production rule
data InterfaceNonAnsiHeader = InterfaceNonAnsiHeader

-- | Incomplete production rule
data InterfaceAnsiHeader = InterfaceAnsiHeader

-- | Incomplete production rule
data ProgramDeclaration = ProgramDeclaration

-- | Incomplete production rule
data ProgramNonAnsiHeader = ProgramNonAnsiHeader

-- | Incomplete production rule
data ProgramAnsiHeader = ProgramAnsiHeader

-- | Incomplete production rule
data CheckerDeclaration = CheckerDeclaration

-- | Incomplete production rule
data ClassDeclaration = ClassDeclaration

-- | Incomplete production rule
data InterfaceClassType = InterfaceClassType

-- | Incomplete production rule
data InterfaceClassDeclaration = InterfaceClassDeclaration

-- | Incomplete production rule
data InterfaceClassItem = InterfaceClassItem

-- | Incomplete production rule
data InterfaceClassMethod = InterfaceClassMethod

-- | Incomplete production rule
data PackageDeclaration = PackageDeclaration

-- | Incomplete production rule
data TimeunitsDeclaration = TimeunitsDeclaration