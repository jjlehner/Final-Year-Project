{-# LANGUAGE TypeApplications #-}

module V2H.IR where

import qualified    Data.Map              as Map
import qualified    V2H.Ast               as Ast

data EdgeIdentifierIR = Posedge | Negedge | Edge deriving (Show)
data EventExpressionIR = EventExpressionIR {
    edgeIdentifier :: EdgeIdentifierIR ,
    connection :: ConnectionIR
} deriving (Show)

data SelectIR = SelectIR deriving (Show, Eq)
data SensitivityIR = Comb | FF [EventExpressionIR] | Latch deriving (Show)
data PackedDimensionIR = PackedDimensionIR deriving (Show, Eq)
data UnpackedDimensionIR = UnpackedDimensionIR deriving (Show, Eq)
data StatementItemIR =  BlockingAssignment ConnectionIR ExpressionIR
                        | NonblockingAssignment ConnectionIR ExpressionIR deriving (Show)
data ExpressionIR = EConnection ConnectionIR deriving (Show)
data DataTypeIR = LogicIR deriving (Show, Eq)
data NetIR = NetIR deriving (Show, Eq)

data ConnectionIR =
    ConnectionVariableIR VariableIR (Maybe SelectIR)
    | ConnectionNetIR NetIR (Maybe SelectIR) deriving (Show, Eq)

data AlwaysConstructIR =
    AlwaysConstructIR {
        identifier                              :: ModuleItemIdentifierIR,
        sensitivity                             :: SensitivityIR,
        inputConnections                        :: [ConnectionIR],
        outputConnections                       :: [ConnectionIR],
        statementItems                          :: [StatementItemIR]
    } deriving (Show)

data VariableIR =
    VariableIR {
        identifier :: ModuleItemIdentifierIR,
        dataType :: DataTypeIR,
        packedDimension :: Maybe PackedDimensionIR,
        unpackedDimension :: Maybe UnpackedDimensionIR
    } deriving (Show, Eq)

newtype ModuleItemIdentifierIR = ModuleItemIdentifierIR String deriving (Show, Ord, Eq)

data IR =
    IR {
        alwaysConstructs   :: Map.Map ModuleItemIdentifierIR AlwaysConstructIR,
        variables :: Map.Map ModuleItemIdentifierIR VariableIR,
        nets :: Map.Map ModuleItemIdentifierIR NetIR
    } deriving (Show)