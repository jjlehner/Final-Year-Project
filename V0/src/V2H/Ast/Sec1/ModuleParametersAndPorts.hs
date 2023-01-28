module V2H.Ast.Sec1.ModuleParametersAndPorts where

import V2H.Ast.Sec2.DeclarationAssignments
import V2H.Ast.Sec8.Primaries
import V2H.Ast.Sec9.Identifiers

data ParameterPorts =
    ParameterPorts {
        parameterAssignments :: [ParameterAssignment],
        parameterPortDeclaration :: [ParameterPortDeclaration]
    } deriving (Show)

-- | Incomplete production rule
data ParameterPortDeclaration = ParameterPortDeclaration deriving (Show)

-- | Incomplete production rule
data PortDeclaration = PortDeclaration deriving (Show)

data Port =
    PUnamed {
        portExpression :: Maybe PortExpression
    } | PNamed {
        portIdentifier :: PortIdentifier,
        portExpression :: Maybe PortExpression
    } deriving (Show)

data PortExpression =
    PortExpression { portReferences :: [PortReference] } deriving (Show)

data PortReference =
    PortReference {
        portIdentifier :: PortIdentifier,
        constantSelect :: ConstantSelect
    } deriving (Show)

data PortDirection = PDInput | PDOutput | PDInout | PDRef deriving (Show)

-- | Incomplete production rule
data NetPortHeader = NetPortHeader deriving (Show)

-- | Incomplete production rule
data VariablePortHeader = VariablePortHeader deriving (Show)

-- | Incomplete production rule
data InterfacePortHeader = InterfacePortHeader deriving (Show)

-- | Incomplete production rule
data AnsiPortDeclaration = AnsiPortDeclaration deriving (Show)