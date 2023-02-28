module V2H.Ast.Sec9.Attributes where

import V2H.Ast.Sec9.Identifiers

-- | Incomplete production rule
data AttributeInstance =
    AttributeInstance{
        attrSpec :: [AttrSpec]
    }   deriving (Show)

-- | Incorrect production rule (Constant Expression should not be Identifier)
data AttrSpec =
    AttrSpec{
        attrName :: AttrName,
        constantExpression :: Identifier
    }   deriving (Show)

newtype AttrName = AttrName Identifier deriving (Show)