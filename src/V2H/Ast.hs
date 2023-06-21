{-# LANGUAGE DuplicateRecordFields #-}
module V2H.Ast where

import GHC.Generics

newtype SourceText = SourceText {
    description :: [Description]
} deriving (Show, Generic)

newtype Description =
    DModuleDeclaration ModuleDeclaration deriving (Show, Generic)

data ModuleDeclaration =
    ModuleDeclaration {
        moduleHeader :: ModuleHeader,
        nonPortModuleItem :: [NonPortModuleItem]
    } deriving (Show, Generic)

data ModuleHeader =
    ModuleHeader {
        moduleIdentifier:: ModuleIdentifier,
        portDeclarations :: Maybe [PortDeclaration]
    } deriving (Show, Generic)

data NonPortModuleItem =
    NPMIDataDeclaration { dataDeclaration :: DataDeclaration }
    | NPMIAlwaysConstruct { alwaysProcedure:: AlwaysConstruct }
    | NPMIModuleInstantiation { moduleInstantiation :: ModuleInstantiation } deriving (Show, Generic)

data AlwaysConstruct =
    ACComb {
        statementItem :: StatementItem
    } | ACFF {
        statementItem :: StatementItem
    } deriving (Show, Generic)
data EventControl =
    EventControl {
        eventExpression :: EventExpression
    } deriving (Show, Generic)

data EventExpression = EE {
    edgeIdentifier :: Maybe EdgeIdentifier,
    expression :: Expression
} | EEList {
    edgeIdentifier :: Maybe EdgeIdentifier,
    expression :: Expression,
    next :: EventExpression
} deriving (Show, Generic)

data EdgeIdentifier = Posedge | Negedge | Edge deriving (Show, Generic)
data StatementItem =
    SIBlockingAssignment {
        blockingAssignment :: BlockingAssignment
    } | SINonblockingAssignment {
        nonblockingAssignment :: NonblockingAssignment
    } | SISeqBlock {
        seqBlock :: SeqBlock
    } | SIConditionalStatement {
        conditionalStatement :: ConditionalStatement
    } | SIProceduralTimingControlStatement {
        eventControl :: EventControl,
        statementItem :: Maybe StatementItem
    } deriving (Show, Generic)


data BlockingAssignment =
    BlockingAssignment {
        variableLvalue :: VariableLvalue,
        expression :: Expression
    } deriving (Show, Generic)

data NonblockingAssignment =
    NonblockingAssignment {
        variableLvalue :: VariableLvalue,
        expression :: Expression
    } deriving (Show, Generic)

data VariableLvalue = VariableLvalue VariableIdentifier (Maybe BitSelect) (Maybe PartSelectRange)
                      | VLConcatenation [VariableLvalue] deriving (Show, Generic)

newtype SeqBlock =
    SeqBlock { statements :: [StatementItem ]} deriving (Show, Generic)


data PortDeclaration =
    PortDeclaration {
        portDirection :: PortDirection,
        netType :: Maybe NetType,
        dataType :: Maybe DataType,
        portIdentifier :: PortIdentifier
    } deriving (Show, Generic)

data DataDeclaration =
    DataDeclaration {
        dataType :: DataType,
        variableIdentifier :: VariableIdentifier
    } deriving (Show, Generic)

data HierarchicalInstance =
    HierarchicalInstance {
        instanceIdentifier :: ModuleInstanceIdentifier,
        portConnections :: [PortConnection]
    } deriving (Show, Generic)

data ModuleInstantiation =
    ModuleInstantiation {
        moduleIdentifier :: ModuleIdentifier,
        hierarchicalInstance :: HierarchicalInstance
    } deriving (Show, Generic)

data PortConnection =
    PCNamed {
        portIdentifier :: PortIdentifier,
        expression :: Expression
    } deriving (Show, Generic)

newtype BitSelect = BitSelect Expression deriving (Show, Generic)
data PartSelectRange = PartSelectRange Integer Integer deriving (Show, Generic)

data ModuleKeyword = MKModule | MKMacromodule deriving (Show, Generic)
data Expression =
    ELiteral Integer
    | EConnection VariableIdentifier (Maybe BitSelect) (Maybe PartSelectRange)
    | EBlank Expression
    | EBinaryOperator BinaryOperator Expression Expression
    | EUnaryOperator UnaryOperator Expression
    | EConcat [Expression] deriving (Show, Generic)

data NetType = NTWire deriving (Show, Generic)
data DataType = DTIntegerVector IntegerVectorType [PackedDimension] | DTString deriving (Show, Generic)
data IntegerVectorType = IVTBit | IVTLogic | IVTReg deriving (Show, Generic)
data PortDirection = PDInput | PDOutput deriving (Show, Generic)
newtype ModuleIdentifier = ModuleIdentifier String deriving (Show, Generic)
newtype PortIdentifier = PortIdentifier String deriving (Show, Generic)
newtype VariableIdentifier = VariableIdentifier String deriving (Show, Generic)
newtype AlwaysConstructIdentifier = AlwaysConstructIdentifier String deriving (Show, Generic)
newtype NetIdentifier = NetIdentifier String deriving (Show, Generic)
newtype ModuleInstanceIdentifier = ModuleInstanceIdentifier String deriving (Show, Generic)
data BinaryOperator = BOPipePipe | BOAmpersandAmpersand | BOEqualEqual | BOPlus | BOMinus | BOAsterisk | BOForwarslash deriving (Show, Generic)
data UnaryOperator = UOExclamationMark | UOTilde deriving (Show, Generic)
newtype PackedDimension = PackedDimension ConstantRange deriving (Show, Generic)
data ConstantRange = ConstantRange Integer Integer deriving (Show, Generic)
data IfBranch = IfBranch Expression StatementItem deriving (Show, Generic)
data ElseIfBranch = ElseIfBranch Expression StatementItem deriving (Show, Generic)
newtype ElseBranch = ElseBranch StatementItem deriving (Show, Generic)
data ConditionalStatement = ConditionalStatement IfBranch [ElseIfBranch] (Maybe ElseBranch) deriving (Show, Generic)