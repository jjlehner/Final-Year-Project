module V2H.Ast.Sec9.Identifiers where

-- Identifier may change to be more complex/not lazy
type Identifier = String

newtype PortIdentifier = PortIdentifier Identifier deriving (Show)
newtype ModuleIdentifier = ModuleIdentifier Identifier deriving (Show)