module V2H.Ast.Sec1.ModuleParametersAndPorts where

import V2H.Ast.Sec2.DeclarationAssignments
import V2H.Ast.Sec8.Primaries
import V2H.Ast.Sec9.Identifiers

data ParameterPorts =
    ParameterPorts {
        parameterAssignments :: [ParameterAssignment],
        parameterPortDeclaration :: [ParameterPortDeclaration]
    }

-- | Incomplete production rule
data ParameterPortDeclaration = ParameterPortDeclaration

-- | Incomplete production rule
data PortDeclaration = PortDeclaration

data Port =
    PUnamed {
        portExpression :: Maybe PortExpression
    } | PNamed {
        portIdentifier :: PortIdentifier,
        portExpression :: Maybe PortExpression
    }
data PortExpression =
    PortExpression { portReferences :: [PortReference] }

data PortReference =
    PortReference {
        portIdentifier :: PortIdentifier,
        constantSelect :: ConstantSelect
    }

data PortDirection = PDInput | PDOutput | PDInout | PDRef

-- | Incomplete production rule
data NetPortHeader = NetPortHeader

-- | Incomplete production rule
data VariablePortHeader = VariablePortHeader

-- | Incomplete production rule
data InterfacePortHeader = InterfacePortHeader

-- | Incomplete production rule
data AnsiPortDeclaration = AnsiPortDeclaration