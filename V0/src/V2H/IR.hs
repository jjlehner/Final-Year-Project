{-# LANGUAGE TemplateHaskell, TypeApplications, DeriveLift,DuplicateRecordFields #-}

module V2H.IR where

import Data.List qualified as List
import Control.Lens
import Data.Maybe qualified as Maybe
import GHC.Generics

import V2H.IR.DataTypes
import V2H.IR.NetTypes
import V2H.IR.Dimensions

import qualified    Data.Map              as Map
import qualified    Data.Set              as Set
import Data.Set
import Data.Function
import GHC.Base

import Language.Haskell.TH.Syntax
import Debug.Trace

newtype AlwaysConstructIdentifierIR = AlwaysConstructIdentifierIR String deriving (Show, Eq, Ord, Generic)
newtype VariableOrNetIdentifierIR = VariableOrNetIdentifierIR String deriving (Show, Eq, Ord, Generic, Lift)
newtype ModuleIdentifierIR = ModuleIdentifierIR String deriving (Show, Eq, Ord, Generic)
newtype ModuleInstanceIdentifierIR = ModuleInstanceIdentifierIR String deriving (Show, Eq, Ord, Generic, Lift)
newtype PortIdentifierIR = PortIdentifierIR String deriving (Show, Eq, Ord, Generic)

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
                        | NonblockingAssignment ConnectionIR ExpressionIR
                        | SeqBlock [StatementItemIR]
                        | ProceduralTimingControlStatement [EventExpressionIR] (Maybe StatementItemIR)
                        | ConditionalStatement ConditionalStatementIR  deriving (Show, Eq, Ord, Generic)

getInputToStatementIR :: StatementItemIR -> [ConnectionIR]
getInputToStatementIR (BlockingAssignment _ expr) =
    getConnectionsInExpression expr
getInputToStatementIR (NonblockingAssignment _ expr) =
    getConnectionsInExpression expr
getInputToStatementIR (ProceduralTimingControlStatement _ statementItem) =
    maybe [] getInputToStatementIR statementItem
getInputToStatementIR (SeqBlock statementItems) =
    concatMap getInputToStatementIR statementItems
getInputToStatementIR (ConditionalStatement conditionalStatement) =
    let elseBranchInputs = fmap getInputToStatementIR conditionalStatement.elseBranch
                                & Maybe.maybeToList
                                & concat
        ifAndElseBranchStatementInputs = concatMap (getInputToStatementIR . snd) (conditionalStatement.ifAndElseIfBranches)
        ifAndElseBranchExpressionInputs = concatMap (getConnectionsInExpression . fst) (conditionalStatement.ifAndElseIfBranches)
    in elseBranchInputs ++ ifAndElseBranchStatementInputs ++ ifAndElseBranchExpressionInputs
getOutputToStatementIR (BlockingAssignment conn _) =
    [conn]
getOutputToStatementIR (NonblockingAssignment conn _) =
    [conn]
getOutputToStatementIR (ProceduralTimingControlStatement _ statementItems) =
    maybe [] getOutputToStatementIR statementItems
getOutputToStatementIR (SeqBlock statementItems) =
    concatMap getOutputToStatementIR statementItems
getOutputToStatementIR (ConditionalStatement conditionalStatement) =
    let elseBranchInputs = fmap getOutputToStatementIR conditionalStatement.elseBranch
                                & Maybe.maybeToList
                                & concat
    in elseBranchInputs ++ concatMap (getOutputToStatementIR . snd) (conditionalStatement.ifAndElseIfBranches)
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
        binaryOpPlus :: DataTypeIR -> SVDataObject -> SVDataObject,
        binaryMinus ::  DataTypeIR -> SVDataObject -> SVDataObject,
        getLSB :: Bool,
        objToInteger :: DataTypeIR -> Integer,
        objToString :: String,
        binaryEqualEqual :: DataTypeIR -> SVDataObject -> SVDataObject,
        binaryAsterisk :: DataTypeIR -> SVDataObject -> SVDataObject,
        isTrue :: Bool,
        isFalse :: Bool,
        cast :: DataTypeIR -> SignalValue
    }

instance Show SVDataObject where
    show = objToString

data SignalValue = SignalValue DataTypeIR SVDataObject deriving (Generic, Show)
toSVDataObject (SignalValue _ svdo) = svdo
instance Eq SignalValue where
    (==) (SignalValue dt1 obj1) (SignalValue dt2 obj2) =
        dt1 == dt2 && isTrue (binaryEqualEqual obj1 dt1 obj2)
instance Ord SignalValue where
    (<=) (SignalValue dt1 obj1) (SignalValue dt2 obj2) =
        objToInteger obj1 dt1 <= objToInteger obj2 dt2
signalValueToInteger (SignalValue dataType svdo) = objToInteger svdo dataType
signalValueToDataType (SignalValue dataType _) = dataType

data ExpressionIR = EConnection ConnectionIR
                        | ELiteral SignalValue
                        | EUnaryOperator UnaryOperatorIR ExpressionIR
                        | EBinaryOperator BinaryOperatorIR ExpressionIR ExpressionIR deriving (Show, Eq, Ord, Generic)

getConnectionsInExpression (EConnection conn) = [conn]
getConnectionsInExpression (EUnaryOperator _ expr) = getConnectionsInExpression expr
getConnectionsInExpression (EBinaryOperator _ expr1 expr2)= getConnectionsInExpression expr1 ++ getConnectionsInExpression expr2
getConnectionsInExpression (ELiteral _) = []

data UnaryOperatorIR =  UOPlus
                        | UOMinus
                        | UOLogicalNot
                        | UOBitwiseNot
                        | UOAmpersand
                        | UOTildaAmpersand
                        | UOPipe
                        | UOTildePipe
                        | UOCaret
                        | UOTildeCaret
                        | UOCaretTilde deriving (Show, Eq, Ord, Generic)

data BinaryOperatorIR = BOPlus
                      | BOMinus
                      | BOAsterisk
                      | BOForwardSlash
                      | BOEqualEqual deriving (Show, Eq, Ord, Generic)
-- Should be changed to CVariableIR and CNetIR, separated as VariableORNetIdentifier will be split into variableIdentifier and NetIdentifier
data ConnectionIR =
    ConnectionVariableIR (HierarchicalIdentifierIR VariableOrNetIdentifierIR) (Maybe SelectIR)
    | ConnectionNetIR (HierarchicalIdentifierIR VariableOrNetIdentifierIR) (Maybe SelectIR) deriving (Show, Eq, Ord, Generic)

fetchMaybeSelectIRFromConnection (ConnectionVariableIR _ select) = select
fetchMaybeSelectIRFromConnection (ConnectionNetIR _ select) = select

-- Could possibly use prisms?
fetchHierarchicalIdentifierFromConnection (ConnectionVariableIR hIden _)= hIden
fetchHierarchicalIdentifierFromConnection (ConnectionNetIR hIden _) = hIden



getSensitiveSignals :: AlwaysConstructIR -> Set.Set (HierarchicalIdentifierIR VariableOrNetIdentifierIR)
getSensitiveSignals alwaysConstructIR =
    case alwaysConstructIR._sensitivity of
        Comb -> Set.map fetchHierarchicalIdentifierFromConnection alwaysConstructIR._inputConnections
        FF eventExpressions -> Set.map extractIdentifierFromEventExpression eventExpressions
        Latch -> Set.map fetchHierarchicalIdentifierFromConnection alwaysConstructIR._inputConnections
    where
        extractIdentifierFromEventExpression eventExpressions =
            fetchHierarchicalIdentifierFromConnection eventExpressions.connection


data VariableIR =
    VariableIR {
        identifier :: VariableOrNetIdentifierIR,
        dataType :: DataTypeIR
    } deriving (Show, Eq, Ord, Generic, Lift)

data NetIR =
    NetIR {
        identifier :: VariableOrNetIdentifierIR,
        netType :: NetTypeIR,
        dataType :: DataTypeIR
    } deriving (Show, Eq, Ord, Generic)


data ModuleItemIdentifierIR = MIIAlways AlwaysConstructIdentifierIR | MIIVariableNet VariableOrNetIdentifierIR deriving (Show, Ord, Eq)

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
        alwaysConstructs    :: [AlwaysConstructIR],
        variables           :: [VariableIR],
        nets                :: [NetIR],
        ports               :: [PortDeclarationIR], -- Should this really be called PortDeclarationIR
        submodules          :: [SubmoduleIR]
    } deriving (Show)

data HierarchicalIdentifierIR a where
    H :: (Ord a, Show a, Eq a) => ModuleInstanceIdentifierIR -> (HierarchicalIdentifierIR a) -> HierarchicalIdentifierIR a
    I :: (Ord a, Show a, Eq a) => a -> HierarchicalIdentifierIR a

deriving instance Show (HierarchicalIdentifierIR a)
deriving instance Eq (HierarchicalIdentifierIR a)
deriving instance Ord (HierarchicalIdentifierIR a)
deriving instance Lift (HierarchicalIdentifierIR VariableOrNetIdentifierIR)

depthOfHierarchicalIdentifier (I _) = 0
depthOfHierarchicalIdentifier (H _ h) = 1 + depthOfHierarchicalIdentifier h

sameRootHierarchicalIdentifier (I x) (I y) = True
sameRootHierarchicalIdentifier (I _) (H _ _) = False
sameRootHierarchicalIdentifier (H _ _) (I _) = False
sameRootHierarchicalIdentifier (H m1 h1) (H m2 h2) = (m1 == m2) && sameRootHierarchicalIdentifier h1 h2

getLeafFromHierachicalIdentifier :: HierarchicalIdentifierIR a -> a
getLeafFromHierachicalIdentifier (H _ b) = getLeafFromHierachicalIdentifier b
getLeafFromHierachicalIdentifier (I i) = i

flattenHierarchicalIdentifier (I _) = []
flattenHierarchicalIdentifier (H m h) = m : flattenHierarchicalIdentifier h

data ExpandedIR =
    ExpandedIR {
        alwaysConstructs    :: Map.Map (HierarchicalIdentifierIR AlwaysConstructIdentifierIR) AlwaysConstructIR,
        variables           :: Map.Map (HierarchicalIdentifierIR VariableOrNetIdentifierIR) VariableIR,
        nets                :: Map.Map (HierarchicalIdentifierIR VariableOrNetIdentifierIR) NetIR,
        connections         :: Map.Map (HierarchicalIdentifierIR VariableOrNetIdentifierIR) [(Maybe SelectIR, ConnectionIR)]
    } deriving (Show, Generic)

findIRFromModuleIdentifier irs iden =
    List.find (\ir -> ir.moduleIdentifier == iden) irs
findIRFromSubmodule irs (SubmoduleIR iden _ _) =
    List.find (\ir -> ir.moduleIdentifier == iden) irs

data AlwaysConstructIR =
    AlwaysConstructIR {
        _alwaysConstructIdentifier               :: AlwaysConstructIdentifierIR,
        _sensitivity                             :: SensitivityIR,
        _inputConnections                        :: Set.Set ConnectionIR,
        _outputConnections                       :: Set.Set ConnectionIR,
        _statementItems                          :: Maybe StatementItemIR
    } deriving (Show, Eq, Ord, Generic)

data ConditionalStatementIR =
    ConditionalStatementIR {
        ifAndElseIfBranches:: [(ExpressionIR, StatementItemIR)],
        elseBranch :: Maybe StatementItemIR
    } deriving (Show, Eq, Ord, Generic)

$(makeLenses ''AlwaysConstructIR)

mkSensitiveProcessMapFromAlwaysConstructIR :: AlwaysConstructIR -> Map.Map (HierarchicalIdentifierIR VariableOrNetIdentifierIR) (Set.Set AlwaysConstructIR)
mkSensitiveProcessMapFromAlwaysConstructIR alwaysConstructIR =
    (, Set.singleton alwaysConstructIR)
    <$> (Set.toList . getSensitiveSignals) alwaysConstructIR
    & Map.fromList

mkSensitiveProcessesMapFromIR :: ExpandedIR -> Map.Map (HierarchicalIdentifierIR VariableOrNetIdentifierIR) (Set.Set AlwaysConstructIR)
mkSensitiveProcessesMapFromIR ir =
    (mkSensitiveProcessMapFromAlwaysConstructIR <$> ir.alwaysConstructs)
    & Map.unionsWith Set.union