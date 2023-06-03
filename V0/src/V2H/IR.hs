{-# LANGUAGE TypeApplications #-}

module V2H.IR where

import Data.List qualified as List
import Control.Lens
import GHC.Generics
import V2H.IR.DataTypes
import V2H.IR.NetTypes
import V2H.IR.Dimensions

import qualified    Data.Map              as Map
import qualified    Data.Set              as Set
import Data.Function
import GHC.Base

type VariableMapIR = Map.Map VariableOrNetIdentifierIR VariableIR
type NetMapIR = Map.Map VariableOrNetIdentifierIR NetIR
type AlwaysConstructMapIR = Map.Map AlwaysConstructIdentifierIR AlwaysConstructIR
type PortDeclarationMapIR = Map.Map PortIdentifierIR PortDeclarationIR

data EdgeIdentifierIR = Posedge | Negedge | Edge deriving (Show, Eq, Ord, Generic)
data EventExpressionIR = EventExpressionIR {
    edgeIdentifier :: Maybe EdgeIdentifierIR ,
    connection :: ConnectionIR
} deriving (Show, Eq, Ord, Generic)

data BitSelectIR = BitSelectIR deriving (Show, Eq, Ord, Generic)
data PartSelectRangeIR = PartSelectRangeIR deriving (Show, Eq, Ord, Generic)

data SelectIR = SelectIR {
    bitSelectIR :: BitSelectIR,
    partSelectRange :: PartSelectRangeIR
} deriving (Show, Eq, Ord, Generic)

data SensitivityIR = Comb | FF (Set.Set EventExpressionIR) | Latch deriving (Show, Eq, Ord, Generic)

data StatementItemIR =  BlockingAssignment ConnectionIR ExpressionIR
                        | NonblockingAssignment ConnectionIR ExpressionIR deriving (Show, Eq, Ord, Generic)

getInputToStatementIR (BlockingAssignment _ expr) =
    getConnectionsInExpression expr
getInputToStatementIR (NonblockingAssignment _ expr) =
    getConnectionsInExpression expr

getOutputToStatementIR (BlockingAssignment conn _) =
    [conn]
getOutputToStatementIR (NonblockingAssignment conn _) =
    [conn]

-- class (Show svdt, Ord svdt, Eq svdt) => SVDataObject svdt where
--     unaryOpMinus :: DataTypeIR -> svdt -> SignalValue
--     unaryOpExclamationMark :: DataTypeIR -> svdt -> SignalValue
--     getLSB :: svdt -> Bool
--     objToInteger :: DataTypeIR -> svdt -> Integer

-- instance SVDataObject Integer where
--     unaryOpMinus dataType a = SignalValue dataType (-a)
--     objToInteger (DTSingular (STScalar SIVTLogic)) a =
--         case a of
--             0 -> 0
--             _ -> 1
--     unaryOpExclamationMark dataType 0 = SignalValue dataType (1::Integer)
--     unaryOpExclamationMark dataType _ = SignalValue dataType (0::Integer)

data SVDataObject =
    SVDataObject {
        unaryOpMinus :: DataTypeIR -> SVDataObject,
        unaryOpExclamationMark :: DataTypeIR -> SVDataObject,
        getLSB :: Bool,
        objToInteger :: DataTypeIR -> Integer,
        objToString :: String,
        binaryEqualEqual :: DataTypeIR -> SVDataObject -> Bool
    }

instance Show SVDataObject where
    show = objToString

data SignalValue = SignalValue DataTypeIR SVDataObject deriving (Generic, Show)
toSVDataObject (SignalValue _ svdo) = svdo
instance Eq SignalValue where
    (==) (SignalValue dt1 obj1) (SignalValue dt2 obj2) =
        dt1 == dt2 && binaryEqualEqual obj1 dt1 obj2
instance Ord SignalValue where
    (<=) (SignalValue dt1 obj1) (SignalValue dt2 obj2) =
        objToInteger obj1 dt1 <= objToInteger obj2 dt2
signalValueToInteger (SignalValue dataType svdo) = objToInteger svdo
signalValueToDataType (SignalValue dataType _) = dataType

data ExpressionIR = EConnection ConnectionIR
                        | ELiteral SignalValue
                        | EUnaryOperator UnaryOperatorIR ExpressionIR deriving (Show, Eq, Ord, Generic)
getConnectionsInExpression (EConnection conn) = [conn]
getConnectionsInExpression (EUnaryOperator _ expr) = getConnectionsInExpression expr
getConnectionsInExpression _ = []

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
                        | UOCaretTilde deriving (Show, Eq, Ord, Generic)

-- Should be changed to CVariableIR and CNetIR
data ConnectionIR =
    ConnectionVariableIR VariableIR (Maybe SelectIR)
    | ConnectionNetIR NetIR (Maybe SelectIR) deriving (Show, Eq, Ord, Generic)

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
    } deriving (Show, Eq, Ord, Generic)

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
    } deriving (Show, Eq, Ord, Generic)

data NetIR =
    NetIR {
        identifier :: VariableOrNetIdentifierIR,
        netType :: NetTypeIR,
        dataType :: DataTypeIR
    } deriving (Show, Eq, Ord, Generic)


data ModuleItemIdentifierIR = MIIAlways AlwaysConstructIdentifierIR | MIIVariableNet VariableOrNetIdentifierIR deriving (Show, Ord, Eq)
newtype AlwaysConstructIdentifierIR = AlwaysConstructIdentifierIR String deriving (Show, Eq, Ord, Generic)
newtype VariableOrNetIdentifierIR = VariableOrNetIdentifierIR String deriving (Show, Eq, Ord, Generic)
newtype ModuleIdentifierIR = ModuleIdentifierIR String deriving (Show, Eq, Ord, Generic)
newtype ModuleInstanceIdentifierIR = ModuleInstanceIdentifierIR String deriving (Show, Eq, Ord, Generic)
newtype PortIdentifierIR = PortIdentifierIR String deriving (Show, Eq, Ord, Generic)
data PortDirectionIR = PDInput | PDOutput deriving (Show, Eq, Ord, Generic)
data PortDeclarationIR =
    PortDeclarationIR {
        portIdentifier :: PortIdentifierIR,
        portDirection :: PortDirectionIR,
        connection :: ConnectionIR
    } deriving (Show, Eq, Ord, Generic)

data SubmoduleIR = SubmoduleIR {
    submoduleIdentifier :: ModuleIdentifierIR,
    submoduleInstanceIdentifier :: ModuleInstanceIdentifierIR,
    connections :: Map.Map PortIdentifierIR ExpressionIR
} deriving (Show, Generic)

data IR =
    IR {
        moduleIdentifier    :: ModuleIdentifierIR,
        alwaysConstructs    :: Map.Map AlwaysConstructIdentifierIR AlwaysConstructIR,
        variables           :: Map.Map VariableOrNetIdentifierIR VariableIR,
        nets                :: Map.Map VariableOrNetIdentifierIR NetIR,
        ports               :: Map.Map PortIdentifierIR PortDeclarationIR,
        submodules          :: [SubmoduleIR]
    } deriving (Show)

data IRExpanded =
    IRExpanded {

    }

findIRFromModuleIdentifier irs iden =
    List.find (\ir -> ir.moduleIdentifier == iden) irs
findIRFromSubmodule irs (SubmoduleIR iden _ _) =
    List.find (\ir -> ir.moduleIdentifier == iden) irs

mkSensitiveProcessMapFromAlwaysConstructIR :: AlwaysConstructIR -> Map.Map VariableOrNetIdentifierIR (Set.Set AlwaysConstructIR)
mkSensitiveProcessMapFromAlwaysConstructIR alwaysConstructIR =
    (, Set.singleton alwaysConstructIR)
    <$> (Set.toList . getSensitiveSignals) alwaysConstructIR
    & Map.fromList

mkSensitiveProcessesMapFromIR :: IR -> Map.Map VariableOrNetIdentifierIR (Set.Set AlwaysConstructIR)
mkSensitiveProcessesMapFromIR ir =
    (mkSensitiveProcessMapFromAlwaysConstructIR <$> Map.elems ir.alwaysConstructs)
    & Map.unionsWith Set.union