{-# LANGUAGE TypeApplications #-}

module V2H.IR where

import V2H.IR.DataTypes
import V2H.IR.NetTypes
import V2H.IR.Dimensions

import qualified    Data.Map              as Map
import qualified    Data.Set              as Set
import qualified    V2H.Ast               as Ast
import Data.Function
data EdgeIdentifierIR = Posedge | Negedge | Edge deriving (Show, Eq, Ord)
data EventExpressionIR = EventExpressionIR {
    edgeIdentifier :: Maybe EdgeIdentifierIR ,
    connection :: ConnectionIR
} deriving (Show, Eq, Ord)

data BitSelectIR = BitSelectIR deriving (Show, Eq, Ord)
data PartSelectRangeIR = PartSelectRangeIR deriving (Show, Eq, Ord)

data SelectIR = SelectIR {
    bitSelectIR :: BitSelectIR,
    partSelectRange :: PartSelectRangeIR
} deriving (Show, Eq, Ord)

data SensitivityIR = Comb | FF (Set.Set EventExpressionIR) | Latch deriving (Show, Eq, Ord)

data StatementItemIR =  BlockingAssignment ConnectionIR ExpressionIR
                        | NonblockingAssignment ConnectionIR ExpressionIR deriving (Show, Eq, Ord)

class (Show svdt, Ord svdt, Eq svdt) => SVDataObject svdt where
    unaryOpMinus :: DataTypeIR -> svdt -> SignalValue
    unaryOpExclamationMark :: DataTypeIR -> svdt -> SignalValue
    getLSB :: svdt -> Bool
    objToInteger :: DataTypeIR -> svdt -> Integer

instance SVDataObject Integer where
    unaryOpMinus dataType a = SignalValue dataType (-a)
    objToInteger (DTSingular (STScalar SIVTLogic)) a =
        case a of
            0 -> 0
            _ -> 1
    unaryOpExclamationMark dataType 0 = SignalValue dataType (1::Integer)
    unaryOpExclamationMark dataType _ = SignalValue dataType (0::Integer)

data SignalValue where
    SignalValue :: (Eq svdt, SVDataObject svdt) => DataTypeIR -> svdt -> SignalValue
deriving instance Show SignalValue

instance Eq SignalValue where
instance Ord SignalValue where

signalValueToInteger (SignalValue dataType svdo) = objToInteger dataType svdo
signalValueToDataType (SignalValue dataType _) = dataType

data ExpressionIR = EConnection ConnectionIR
                        | ELiteral SignalValue
                        | EUnaryOperator UnaryOperatorIR ExpressionIR deriving (Show, Eq, Ord)

data UnaryOperatorIR =  UOPlus
                        | UOMinus
                        | UOExclamationMark
                        | UOTilda
                        | UOAmpersand
                        | UOTildaAmpersand
                        | UOPipe
                        | UOTildePipe
                        | UOCaret
                        | UOTildeCaret
                        | UOCaretTilde deriving (Show, Eq, Ord)

data ConnectionIR =
    ConnectionVariableIR VariableIR (Maybe SelectIR)
    | ConnectionNetIR NetIR (Maybe SelectIR) deriving (Show, Eq, Ord)

-- Could possibly use prisms?
fetchNetVariableIdentifierFromConnection (ConnectionVariableIR var _) = var.identifier
fetchNetVariableIdentifierFromConnection (ConnectionNetIR net _) = net.identifier

data AlwaysConstructIR =
    AlwaysConstructIR {
        identifier                              :: AlwaysConstructIdentifierIR,
        sensitivity                             :: SensitivityIR,
        inputConnections                        :: Set.Set ConnectionIR,
        outputConnections                       :: Set.Set ConnectionIR,
        statementItems                          :: [StatementItemIR]
    } deriving (Show, Eq, Ord)

getSensitiveSignals :: AlwaysConstructIR -> Set.Set VariableOrNetIdentifierIR
getSensitiveSignals alwaysConstructIR =
    case alwaysConstructIR.sensitivity of
        Comb -> Set.map extractIdentifierFromConnections alwaysConstructIR.inputConnections
        FF eventExpressions -> Set.map extractIdentifierFromEventExpression eventExpressions
        Latch -> Set.map extractIdentifierFromConnections alwaysConstructIR.inputConnections
    where
        extractIdentifierFromConnections inputConnections =
            case inputConnections of
                ConnectionVariableIR v _ -> v.identifier
                ConnectionNetIR i _ -> i.identifier
        extractIdentifierFromEventExpression eventExpressions =
            extractIdentifierFromConnections eventExpressions.connection


data VariableIR =
    VariableIR {
        identifier :: VariableOrNetIdentifierIR,
        dataType :: DataTypeIR
    } deriving (Show, Eq, Ord)

data NetIR =
    NetIR {
        identifier :: VariableOrNetIdentifierIR,
        netType :: NetTypeIR,
        dataType :: DataTypeIR
    } deriving (Show, Eq, Ord)


data ModuleItemIdentifierIR = MIIAlways AlwaysConstructIdentifierIR | MIIVariableNet VariableOrNetIdentifierIR deriving (Show, Ord, Eq)
newtype AlwaysConstructIdentifierIR = AlwaysConstructIdentifierIR String deriving (Show, Eq, Ord)
newtype VariableOrNetIdentifierIR = VariableOrNetIdentifierIR String deriving (Show, Eq, Ord)

data IR =
    IR {
        alwaysConstructs    :: Map.Map AlwaysConstructIdentifierIR AlwaysConstructIR,
        variables           :: Map.Map VariableOrNetIdentifierIR VariableIR,
        nets                :: Map.Map VariableOrNetIdentifierIR NetIR
    } deriving (Show)

mkSensitiveProcessMapFromAlwaysConstructIR :: AlwaysConstructIR -> Map.Map VariableOrNetIdentifierIR (Set.Set AlwaysConstructIR)
mkSensitiveProcessMapFromAlwaysConstructIR alwaysConstructIR =
    (, Set.singleton alwaysConstructIR)
    <$> (Set.toList . getSensitiveSignals) alwaysConstructIR
    & Map.fromList

mkSensitiveProcessesMapFromIR :: IR -> Map.Map VariableOrNetIdentifierIR (Set.Set AlwaysConstructIR)
mkSensitiveProcessesMapFromIR ir =
    (mkSensitiveProcessMapFromAlwaysConstructIR <$> Map.elems ir.alwaysConstructs)
    & Map.unionsWith Set.union