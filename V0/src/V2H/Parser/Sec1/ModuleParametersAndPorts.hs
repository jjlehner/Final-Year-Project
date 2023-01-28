module V2H.Parser.Sec1.ModuleParametersAndPorts where

import V2H.Ast.Sec1.ModuleParametersAndPorts
import V2H.Parser.Sec8.Primaries
import V2H.Parser.Sec9.Identifiers
import Text.Parsec

singleton nt = do
    res <- nt
    return [res]

-- | Incomplete production rule
-- This is wrong
parameterPortsNT :: ParserSV ParameterPorts
parameterPortsNT = pure $ ParameterPorts [] []

-- | Incomplete production rule
parameterPortDeclarationNT :: ParserSV ParameterPortDeclaration
parameterPortDeclarationNT = pure ParameterPortDeclaration

-- | Incomplete production rule
portDeclarationNT :: ParserSV PortDeclaration
portDeclarationNT = undefined

-- | Incomplete production rule
portNT :: ParserSV Port
portNT = PUnamed <$> optionMaybe portExpressionNT
         <|> PNamed <$> portIdentifierNT <*> optionMaybe portExpressionNT

-- | Incomplete production rule
portExpressionNT :: ParserSV PortExpression
portExpressionNT = PortExpression <$> singleton portReferenceNT

-- | Incomplete production rule
portReferenceNT :: ParserSV PortReference
portReferenceNT = PortReference <$> portIdentifierNT <*> constantSelectNT

portDirectionNT :: ParserSV PortDirection
portDirectionNT = undefined

-- | Incomplete production rule
netPortHeaderNT :: ParserSV NetPortHeader
netPortHeaderNT = undefined

-- | Incomplete production rule
variablePortHeaderNT :: ParserSV VariablePortHeader
variablePortHeaderNT = undefined

-- | Incomplete production rule
interfacePortHeaderNT :: ParserSV InterfacePortHeader
interfacePortHeaderNT = undefined

-- | Incomplete production rule
ansiPortDeclarationNT :: ParserSV AnsiPortDeclaration
ansiPortDeclarationNT = undefined