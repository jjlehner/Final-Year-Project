module V2H.Ast where
import           GHC.Generics
----- Sec 1 -----
---- 1.1 - Library Source Text ----
-- | Incomplete production rule
data LibraryText = LibraryText deriving (Show, Generic)

-- | Incomplete production rule
data LibraryDescription = LibraryDescription deriving (Show, Generic)

-- | Incomplete production rule
data LibraryDeclaration = LibraryDeclaration deriving (Show, Generic)

-- | Incomplete production rule
data IncludeStatement = IncludeStatement deriving (Show, Generic)

---- 1.2 - SystemVerilog Source Text ----

data SourceText = SourceText {
    timeunitsDeclaration :: Maybe TimeunitsDeclaration,
    description          :: [Description]
} deriving (Show, Generic)

data Description =
    DModuleDeclaration {
        moduleDeclaration :: ModuleDeclaration
    } | DUdpDeclaration {
        udpDeclaration :: UdpDeclaration
    } | DInterfaceDeclaration {
        interfaceDeclaration :: InterfaceDeclaration
    } | DProgramDeclaration {
        programDeclaration :: ProgramDeclaration
    } | DPackageDeclaration {
        packageDeclaration :: PackageDeclaration
    } | DPackageItem {
        attributeInstances :: [AttributeInstance],
        packageItem        :: PackageItem
    } | DBindDirective {
        attributeInstances :: [AttributeInstance],
        bindDirective      :: BindDirective
    } | DConfigDeclaration {
        configDeclaration :: ConfigDeclaration
    } deriving (Show, Generic)

data ModuleNonAnsiHeader =
    ModuleNonAnsiHeader {
        attributeInstances        :: [AttributeInstance],
        moduleKeyword             :: ModuleKeyword,
        lifetime                  :: Maybe Lifetime,
        moduleIdentifier          :: ModuleIdentifier,
        packageImportDeclarations :: [PackageImportDeclaration],
        parameterPorts            :: Maybe ParameterPorts,
        ports                     :: [Port]
    } deriving (Show, Generic)

data ModuleAnsiHeader =
    ModuleAnsiHeader {
        attributeInstances        :: [AttributeInstance],
        moduleKeyword             :: ModuleKeyword,
        lifetime                  :: Maybe Lifetime,
        moduleIdentifier          :: ModuleIdentifier,
        packageImportDeclarations :: [PackageImportDeclaration],
        parameterPorts            :: Maybe ParameterPorts,
        portDeclarations          :: Maybe PortDeclarations
    } deriving (Show, Generic)

data ModuleDeclaration =
    MDNonAnsiHeader {
        moduleNonAnsiHeader   :: ModuleNonAnsiHeader,
        timeunitsDeclarations :: Maybe TimeunitsDeclaration,
        moduleItems           :: [ModuleItem]
    } | MDAnsiHeader {
        moduleAnsiHeader      :: ModuleAnsiHeader,
        timeunitsDeclarations :: Maybe TimeunitsDeclaration,
        nonPortModuleItems    :: [NonPortModuleItem]
    } | MDAttributes {
        attributeInstances   :: [AttributeInstance],
        lifetime             :: Maybe Lifetime,
        moduleIdentifier     :: ModuleIdentifier,
        timeunitsDeclaration :: Maybe TimeunitsDeclaration,
        moduleItems          :: [ModuleItem]
    } | MDNonAnsiExtern {
        moduleNonAnsiHeader :: ModuleNonAnsiHeader
    } | MDAnsiExtern {
        moduleAnsiHeader :: ModuleAnsiHeader
    } deriving (Show, Generic)

data ModuleKeyword = Module | Macromodule deriving (Show, Generic)

-- | Incomplete production rule
data InterfaceDeclaration = InterfaceDeclaration deriving (Show, Generic)

-- | Incomplete production rule
data InterfaceNonAnsiHeader = InterfaceNonAnsiHeader deriving (Show, Generic)

-- | Incomplete production rule
data InterfaceAnsiHeader = InterfaceAnsiHeader deriving (Show, Generic)

-- | Incomplete production rule
data ProgramDeclaration = ProgramDeclaration deriving (Show, Generic)

-- | Incomplete production rule
data ProgramNonAnsiHeader = ProgramNonAnsiHeader deriving (Show, Generic)

-- | Incomplete production rule
data ProgramAnsiHeader = ProgramAnsiHeader deriving (Show, Generic)

-- | Incomplete production rule
data CheckerDeclaration = CheckerDeclaration deriving (Show, Generic)

-- | Incomplete production rule
data ClassDeclaration = ClassDeclaration deriving (Show, Generic)

-- | Incomplete production rule
data InterfaceClassType = InterfaceClassType deriving (Show, Generic)

-- | Incomplete production rule
data InterfaceClassDeclaration = InterfaceClassDeclaration deriving (Show, Generic)

-- | Incomplete production rule
data InterfaceClassItem = InterfaceClassItem deriving (Show, Generic)

-- | Incomplete production rule
data InterfaceClassMethod = InterfaceClassMethod deriving (Show, Generic)

-- | Incomplete production rule
data PackageDeclaration = PackageDeclaration deriving (Show, Generic)

-- | Incomplete production rule
data TimeunitsDeclaration = TimeunitsDeclaration {} deriving (Show, Generic)

---- 1.3 - Module parameters and Ports ----
data ParameterPorts =
    ParameterPorts {
        parameterAssignments     :: [ParamAssignment],
        parameterPortDeclaration :: [ParameterPortDeclaration]
    } deriving (Show, Generic)

-- | Incomplete production rule
data ParameterPortDeclaration =
    PPD{
        parameterDeclaration :: ParameterDeclaration
    } | PPDLocal {
        localParameterDeclaration :: LocalParameterDeclaration
    } | PPDDataTypeParamAssignments {
        dataType             :: DataType,
        parameterAssignments :: [ParamAssignment]
    } | PPDTypeAssignments {
        typeAssignments :: [TypeAssignment]
    } deriving (Show, Generic)

-- | Incomplete production rule
-- Referred to as list_of_port_declarations in System Verilog Official Grammar
type PortDeclarations = [PortDeclarationsItem]
data PortDeclarationsItem =
    PortDeclarationsItem {
        attributeInstances  :: [AttributeInstance],
        ansiPortDeclaration :: AnsiPortDeclaration
    } deriving (Show, Generic)

-- | Incomplete production rule
data PortDeclaration =
    PDInputDeclaration {
        attributeInstances :: [AttributeInstance],
        inputDeclaration   :: InputDeclaration
    } deriving (Show, Generic)

data Port =
    PUnamed {
        portExpression :: Maybe PortExpression
    } | PNamed {
        portIdentifier :: PortIdentifier,
        portExpression :: Maybe PortExpression
    } deriving (Show, Generic)

data PortExpression =
    PortExpression { portReferences :: [PortReference] } deriving (Show, Generic)

data PortReference =
    PortReference {
        portIdentifier :: PortIdentifier,
        constantSelect :: ConstantSelect
    } deriving (Show, Generic)

data PortDirection = PDInput | PDOutput | PDInout | PDRef deriving (Show, Generic)

data NetPortHeader = NetPortHeader {
        portDirection :: Maybe PortDirection,
        netPortType   :: NetPortType
    } deriving (Show, Generic)

data VariablePortHeader = VariablePortHeader {
        portDirection    :: Maybe PortDirection,
        variablePortType :: VariablePortType
    } deriving (Show, Generic)

-- | Incomplete production rule
data InterfacePortHeader =
    IPDNamed {
        interfaceIdentifier :: InterfaceIdentifier,
        modportIdentifier   :: Maybe ModportIdentifier
    } | IPDAnonymous {
        modportIdentifier :: Maybe ModportIdentifier
    } deriving (Show, Generic)

-- | Incomplete production rule
data AnsiPortDeclaration =
    APDNetOrInterfaceHeader {
        netOrInterfacePortHeader :: Maybe (Either NetPortHeader InterfacePortHeader),
        portIdentifier :: PortIdentifier,
        unpackedDimensions :: [UnpackedDimension],
        constantExpression :: Maybe ConstantExpression
    } | APDVariableHeader {
        variablePortHeader :: Maybe VariablePortHeader,
        portIdentifier     :: PortIdentifier,
        variableDimensions :: [VariableDimension],
        constantExpression :: Maybe ConstantExpression
    } | APDDirection {
        portDirection  :: Maybe PortDirection,
        portIdentifier :: PortIdentifier,
        expression     :: Maybe Expression
    } deriving (Show, Generic)
---- 1.4 - Module Items ----
-- | Incomplete production rule
data BindDirective = BindDirective deriving (Show, Generic)

data ModuleCommonItem =
    MCIModuleOrGenerateItemDeclaration {
        moduleOrGenerateItemDeclaration :: ModuleOrGenerateItemDeclaration
    } | MCIInterfaceInstantiation {
        interfaceInstantiation :: InterfaceInstantiation
    } | MCIProgramInstantiation {
        programInstantiation :: ProgramInstantiation
    } | MCIAssertionItem {
        assertionItem :: AssertionItem
    } | MCIBindDirective {
        bindDirective :: BindDirective
    } | MCIContinuousAssign {
        continuousAssign :: ContinuousAssign
    } | MCINetAlias {
        netAlias :: NetAlias
    } | MCIInitialConstruct {
        initialConstruct :: InitialConstruct
    } | MCIFinalConstruct {
        finalConstruct :: FinalConstruct
    } | MCIAlwaysConstruct {
        alwaysConstruct :: AlwaysConstruct
    } | MCILoopGenerateConstruct {
        loopGenerateConstruct :: LoopGenerateConstruct
    } | MCIConditionalGenerateConstruct {
        conditionalGenerateConstruct :: ConditionalGenerateConstruct
    } | MCIElaborationSystemTask {
        elaborationSystemTask :: ElaborationSystemTask
    } deriving (Show, Generic)

data ModuleItem =
    MIPort_Declaration {
        portDeclaration :: PortDeclaration
    } | MINonPortModuleItem {
        nonPortModuleItem :: NonPortModuleItem
    } deriving (Show, Generic)

-- | Incomplete production rule
data ModuleOrGenerateItem = MOGIModuleCommonItem{
        attributeInstances :: [AttributeInstance],
        moduleCommonItem   :: ModuleCommonItem
    } deriving (Show, Generic)

data NonPortModuleItem =
    NPMIGenerateRegion {
        generateRegion :: GenerateRegion
    } | NPMIModuleOrGenerateItem {
        moduleOrGenerateItem :: ModuleOrGenerateItem
    } | NPMISpecifyBlock {
        specifyBlock :: SpecifyBlock
    } | NPMISpecparamDeclaration {
        attributeInstances :: [AttributeInstance]
    } | NPMIProgramDeclaration {
        programDeclaration :: ProgramDeclaration
    } | NPMIModuleDeclaration {
        moduleDeclaration :: ModuleDeclaration
    } | NPMIInterfaceDeclaration {
        interfaceDeclaration :: InterfaceDeclaration
    } | NPMITimeunitsDeclaration {
        timeunitsDeclaration :: TimeunitsDeclaration
    } deriving (Show, Generic)

-- | Incomplete production rule
data ModuleOrGenerateItemDeclaration = MOGIDPackageOrGenerateItemDeclaration {
    packageOrGenerateItemDeclaration :: PackageOrGenerateItemDeclaration
} deriving (Show, Generic)

-- | Incomplete production rule
data ElaborationSystemTask = ElaborationSystemTask deriving (Show, Generic)

---- 1.5 - Configuration Source Text ----
-- | Incomplete production rule
data ConfigDeclaration = ConfigDeclaration deriving (Show, Generic)

---- 1.6 - Interface Items ----
---- 1.7 - Program Items ----
---- 1.8 - Checker Items ----
---- 1.9 - Class Items ----
---- 1.10 - Constraints ----
---- 1.11 - Package Items ----
-- | Incomplete production rule
data PackageItem = PackageItem deriving (Show, Generic)
data PackageOrGenerateItemDeclaration = POGIDData {
    dataDeclaration :: DataDeclaration
} deriving (Show, Generic)
----- Sec 2 -----
---- 2.1.1 - Module Parameter Declarations ----
-- | Incomplete production rule
data LocalParameterDeclaration = LPDParamAssignments {
        dataTypeOrImplicit :: DataTypeOrImplicit,
        paramAssignments   :: [ParamAssignment]
    } | LPDTypeAssignments {
        typeAssignments :: [TypeAssignment]
    } deriving (Show, Generic)

data ParameterDeclaration =
    PDDataTypeOrImplicit {
        dataTypeOrImplicit :: DataTypeOrImplicit,
        paramAssignments   :: [ParamAssignment]
    }
    | PDTypeAssignments {
        typeAssignments :: [TypeAssignment]
    } deriving (Show, Generic)

-- | Incomplete production rule
data SpecparamDeclaration = SpecparamDeclaration deriving (Show, Generic)

---- 2.1.2 - Port Declarations ----

data InputDeclaration =
    IDNetIdentifiers {
        netPortType     :: NetPortType,
        portIdentifiers :: [PortIdentifier]
    } | IDVariableIdentifiers {
        variablePortType    :: VariablePortType,
        variableIdentifiers :: [VariableIdentifier]
    } deriving (Show, Generic)

---- 2.1.3 - Type Declarations ----
-- | Incomplete production rule
data DataDeclaration = DD {
    const                   :: Bool,
    var                     :: Bool,
    lifetime                :: Maybe Lifetime,
    dataTypeOrImplicit      :: DataTypeOrImplicit,
    variableDeclAssignments :: [VariableDeclAssignment]
} deriving (Show, Generic)

data PackageImportItem =
    PIIIdentifier {
        packageIdentifier :: PackageIdentifier,
        identifier        :: Identifier
    } | PIIWildcard {
        packageIdentifier :: PackageIdentifier
    } deriving (Show, Generic)

data Lifetime = Static | Automatic deriving (Show, Generic)

-- | Incomplete production rule
data PackageImportDeclaration = PackageImportDeclaration [PackageImportItem] deriving (Show, Generic)
---- 2.2.1 - Net and Variable Types ----
data DataType = DTIntegerVector {
        integerVectorType :: IntegerVectorType,
        signing           :: Maybe Signing,
        packedDimension   :: [PackedDimension]
    } deriving (Show, Generic)

type DataTypeOrImplicit = Either DataType ImplicitDataType
data ImplicitDataType =
    ImplicitDataType {
        signing          :: Maybe Signing,
        packedDimensions :: [PackedDimension]
    } deriving (Show, Generic)

data ClassScope = ClassScope { classType :: ClassType } deriving (Show, Generic)
data ClassType =
    ClassType {
        psClassIdentifier :: PsClassIdentifier,
        parameterValueAssignment :: Maybe ParameterValueAssignment,
        classIdentifierParameterValueAssignments :: [ClassIdentifierParameterValueAssignment]
    } deriving (Show, Generic)

data ClassIdentifierParameterValueAssignment =
    ClassIdentifierParameterValueAssignment {
        classIdentifier :: ClassIdentifier,
        parameterValueAssignment :: Maybe ParameterValueAssignment
    } deriving (Show, Generic)

data IntegerType =
    ITVector   { integerVectorType :: IntegerVectorType }
    | ITAtom   { integerAtomType :: IntegerAtomType } deriving (Show, Generic)

data IntegerAtomType = IATByte | IATShortint | IATInt | IATLongint | IATInteger | IATTime deriving (Show, Generic)

data IntegerVectorType = IVTBit | IVTLogic | IVTReg deriving (Show, Generic)
data NonIntegerType = NITShortreal | NITReal | NITRealtime deriving (Show, Generic)

data NetType = Supply0 | Supply1 | Tri | Triand | Trior | Trireg| Tri0 | Tri1 | Uwire| Wire | Wand | Wor deriving (Show, Generic)

-- | Incomplete production rule
data NetPortType =
    NPTDataOrImplicit {
        netType            :: Maybe NetType,
        dataTypeOrImplicit :: DataTypeOrImplicit
    } deriving (Show, Generic)

data VariablePortType = VariablePortType VarDataType deriving (Show, Generic)
data VarDataType =
    VDT { dataType :: DataType }
    | VDTOrImplicit { dataTypeOrImplicit :: DataTypeOrImplicit } deriving (Show, Generic)

data Signing = SSigned | SUnsigned deriving (Show, Generic)
data SimpleType =
    STInteger { integerType :: IntegerType }
    | STNonInteger { nonIntegerType :: NonIntegerType }
    | STPsIdentifier { psTypeIdentifier :: PsTypeIdentifier }
    | STPsParameterIdentifier { psParameterIdentifier :: PsParameterIdentifier } deriving (Show, Generic)

data TypeReference =
    TRExpression {
        expression :: Expression
    } | TRDataType {
        dataType :: DataType
    } deriving (Show, Generic)

---- 2.2.2 - Strengths ----
data DriveStrength = DriveStrength deriving (Show, Generic)

---- 2.2.3 - Delays ----
---- 2.3 - Declarations Lists ----
---- 2.4 - Declarations Assignments ----
-- | Incomplete production rule
data DefparamAssignment = DefparamAssignment deriving (Show, Generic)

-- | Incomplete production rule
data ParamAssignment = ParamAssignment {
        parameterIdentifier     :: ParameterIdentifier,
        unpackedDimensions      :: [UnpackedDimension],
        constantParamExpression :: Maybe ConstantParamExpression
    } deriving (Show, Generic)

-- | Incomplete production rule
data SpecparamAssignment = SpecparamAssignment deriving (Show, Generic)

-- | Incomplete production rule
data TypeAssignment =
    TypeAssignment {
        identifier :: TypeIdentifier,
        dataType   :: Maybe DataType
    } deriving (Show, Generic)

-- | Incomplete production rule
data PulseControlSpecparam = PulseControlSpecparam deriving (Show, Generic)

-- | Incomplete production rule
data ErrorLimitValue = ErrorLimitValue deriving (Show, Generic)

-- | Incomplete production rule
data LimitValue = LimitValue deriving (Show, Generic)

-- | Incomplete production rule
data VariableDeclAssignment = VDA {
    variableIdentifier :: VariableIdentifier,
    variableDimensions :: [VariableDimension],
    expression         :: Maybe Expression
} deriving (Show, Generic)

-- | Incomplete production rule
data ClassNew = ClassNew deriving (Show, Generic)

-- | Incomplete production rule
data DynamicArrayNew = DynamicArrayNew deriving (Show, Generic)

---- 2.5 - Declaration Ranges ----
data UnpackedDimension =
    UDConstantRange {
        constantRange :: ConstantRange
    } | UDConstantExpression {
        constantExpression :: ConstantExpression
    } deriving (Show, Generic)

data PackedDimension =
    PDConstantRange {
        constantRange :: ConstantRange
    } | PDUnsized {
        unsizedDimension :: UnsizedDimension
    } deriving (Show, Generic)
-- | Incomplete production rule
data VariableDimension = VDUnsized UnsizedDimension | VDUnpacked UnpackedDimension deriving (Show, Generic)
data UnsizedDimension = UnsizedDimension deriving (Show, Generic)
---- 2.6 - Function Declarations ----
---- 2.7 - Task Declarations ----
---- 2.8 - Block Item Declarations ----

data BlockItemDeclaration = BIDData {
    attributeInstances :: [AttributeInstance],
    dataDeclaration :: DataDeclaration
} deriving (Show, Generic)

---- 2.9 - Interface Declarations ----
---- 2.10 - Assertion Declarations ----
---- 2.11 - Covergroup Declarations ----

---- Sec 3 ----
---- 3.1 - Primitive Instantiation And Instances ----
---- 3.2 - Primitive Strengths ----
---- 3.3 - Primitive Terminals ----
---- 3.4 - Primitive Gate And Switch Types ----

---- Sec 4 ----
---- 4.1.1 - Module Instantiation ----
data ParameterValueAssignment = ParameterValueAssignment {
    parameterAssignments :: Maybe ParameterAssignments
} deriving (Show, Generic)
data ParameterAssignments = PAOrdered {
    orderedParameterAssignments :: [OrderedParameterAssignment]
} | PANamed {
    namedParameterAssignments :: [NamedParameterAssignment]
} deriving (Show, Generic)

data OrderedParameterAssignment = OrderedParameterAssignment {
    paramExpression :: ParamExpression
} deriving (Show, Generic)

data NamedParameterAssignment = NamedParameterAssignment {
    parameterIdentifier :: ParameterIdentifier,
    paramExpression :: Maybe ParamExpression
} deriving (Show, Generic)
---- 4.1.2 - Interface Instantiation ----
data InterfaceInstantiation = InterfaceInstantiation deriving (Show, Generic)
---- 4.1.3 - Program Instantiation ----
data ProgramInstantiation = ProgramInstantiation deriving (Show, Generic)
---- 4.1.4 - Checker Instantiation ----
---- 4.2 - Generated Instantiation ----
data GenerateRegion = GenerateRegion {
    generateIterms :: [GenerateItem]
} deriving (Show, Generic)

-- | Incomplete production rule
data LoopGenerateConstruct = LoopGenerateConstruct deriving (Show, Generic)

-- | Incomplete production rule
data ConditionalGenerateConstruct = ConditionalGenerateConstruct deriving (Show, Generic)

-- | Incomplete production rule
data GenerateItem = GenerateItem deriving (Show, Generic)

---- Sec 5 ----
---- 5.1 - UDP Declaration ----
-- | Incomplete production rule
data UdpNonAnsiDeclaration = UdpNonAnsiDeclaration deriving (Show, Generic)

-- | Incomplete production rule
data UdpAnsiDeclaration = UdpAnsiDeclaration deriving (Show, Generic)

-- | Incomplete production rule
data UdpDeclaration = UdpDeclaration deriving (Show, Generic)
---- 5.2 - UDP Ports ----
---- 5.3 - UDP Body ----
---- 5.4 - UDP Instantiation ----

---- Sec 6 -----
---- 6.1 - Continuous Assignment And Net Alias Statements ----
data ContinuousAssign = ContinuousAssign deriving (Show, Generic)

-- | Incomplete production rule
data NetAlias = NetAlias deriving (Show, Generic)

-- | Incomplete production rule
data NetAssignment = NetAssignment deriving (Show, Generic)
---- 6.2 - Procedural Blocks And Assignments ----
-- | Incomplete production rule
data InitialConstruct = InitialConstruct deriving (Show, Generic)

-- | Incomplete production rule
data AlwaysConstruct =
    AlwaysConstruct {
        alwaysKeyword :: AlwaysKeyword,
        statement     :: Statement
    } deriving (Show, Generic)

-- | Incomplete production rule
data FinalConstruct = FinalConstruct deriving (Show, Generic)

data AlwaysKeyword = Always | AlwaysComb | AlwaysLatch | AlwaysFf deriving (Show, Generic)

-- | Incomplete production rule
data BlockingAssignment = BAOperator { operatorAssignment :: OperatorAssignment }deriving (Show, Generic)

-- | Incomplete production rule
data OperatorAssignment = OperatorAssignment {
    variableLvalue :: VariableLvalue,
    assignmentOperator :: AssignmentOperator,
    expression :: Expression
} deriving (Show, Generic)

-- | Incomplete production rule
data AssignmentOperator =
    AOEqual
    | AOPlusEqual
    | AOMinusEqual
    | AOAsteriskEqual
    | AOForwardslashEqual
    | AOPercentageEqual
    | AOAmpersandEqual
    | AOPipeEqual
    | AOCaretEqual
    | AOLesserLesserEqual
    | AOGreaterGreaterEqual
    | AOLesserLesserLesserEqual
    | AOGreaterGreaterGreaterEqual deriving (Show, Generic)

data NonblockingAssignment =
    NonblockingAssignment {
        variableLvalue :: VariableLvalue,
        expression :: Expression
    } deriving (Show, Generic)

-- | Incomplete production rule
data ProceduralContinuousAssignment = ProceduralContinuousAssignment deriving (Show, Generic)

-- | Incomplete production rule
data VariableAssignment = VariableAssignment deriving (Show, Generic)
---- 6.3 - Parallel And Sequential Blocks ----

data SeqBlock = SeqBlock {
    blockIdentifier :: Maybe BlockIdentifier,
    blockItemDeclarations :: [BlockItemDeclaration],
    statementsOrNull :: [StatementOrNull],
    startBlockIdentifier :: Maybe BlockIdentifier,
    endBlockIdentifier :: Maybe BlockIdentifier
} deriving (Show, Generic)

---- 6.4 - Statements ----
-- | Incomplete production rule
type StatementOrNull = Either Statement [AttributeInstance]

-- | Incomplete production rule
data Statement =
    Statement {
        blockIdentifier :: Maybe BlockIdentifier,
        attributeInstances :: [AttributeInstance],
        statementItem :: StatementItem
    } deriving (Show, Generic)

-- | Incomplete production rule
data StatementItem =
    SIBlockingAssignment {
        blockingAssignment :: BlockingAssignment
    } | SINonblockingAssignment {
        nonblockingAssignment :: NonblockingAssignment
    } | SIProceduralTimingControlStatement {
        proceduralTimingControlStatement :: ProceduralTimingControlStatement
    } | SISeqBlock {
        seqBlock :: SeqBlock
    } deriving (Show, Generic)

-- | Incomplete production rule
data FunctionStatement = FunctionStatement deriving (Show, Generic)

-- | Incomplete production rule
data FunctionStatementOrNull = FunctionStatementOrNull deriving (Show, Generic)

-- | Incomplete production rule
data VariableIdentifierList = VariableIndentifierList deriving (Show, Generic)
---- 6.5 - Timing Control Statements ----
data ProceduralTimingControlStatement =
    ProceduralTimingControlStatement {
        proceduralTimingControl :: ProceduralTimingControl,
        statementOrNull :: StatementOrNull
    } deriving (Show, Generic)

-- | Incomplete production rule
data ProceduralTimingControl =
    PTCEvent {
        eventControl :: EventControl
    } deriving (Show, Generic)


data EventControl =
    ECExpression {
        eventExpression :: EventExpression
    } | ECAsterisk deriving (Show, Generic)

data EventExpression =
    EE {
        edgeIdentifier :: Maybe EdgeIdentifier,
        expression :: Expression,
        iffExpression :: Maybe Expression
    } deriving (Show, Generic)
---- 6.6 - Conditional Statements ----
---- 6.7 - Case Statements ----
---- 6.7.1 - Patterns ----

data AssignmentPatternExpressionType =
    APETPsTypeIdentifier {
        psTypeIdentifier :: PsTypeIdentifier
    } | APETPsParameterIdentifier {
        psParameterIdentifier :: PsParameterIdentifier
    } | APETIntegerAtomType {
        integerAtomType :: IntegerAtomType
    } | APETTypeReference {
        typeReference :: TypeReference
    } deriving (Show, Generic)

data AssignmentPatternVariableLvalue =
    AssignmentPatternVariableLvalue {
        variableLvalues :: [VariableLvalue]
    } deriving (Show, Generic)

---- 6.8 - Looping Statements ----
---- 6.9 - Subroutine Call Statements ----
---- 6.10 - Assertion Statements ----
-- | Incomplete production rule
data AssertionItem = AssertionItem deriving (Show, Generic)
---- 6.11 - Clocking Block ----
---- 6.12 - Randsequence ----

---- Sec 7 ----
---- 7.1 - Specify Block Declaration ----
data SpecifyBlock = SpecifyBlock deriving (Show, Generic)
---- 7.2 - Specify Path Declarations ----
---- 7.3 - Specify Block Terminals ----
---- 7.4 - Specify Path Delays ----

data EdgeIdentifier = EIPosedge | EINegedge | EIEdge deriving (Show, Generic)

---- 7.5 - System Timing Checks ----
---- 7.5.2 - System Timing Check Command Arguments ----
---- 7.5.3 - System Timing Check Event Definitions ----

---- Sec 8 ----
---- 8.1 - Concatenations ----
data StreamingConcatenation =
    StreamingConcatenation {
        streamOperator :: StreamOperator,
        sliceSize :: Maybe SliceSize,
        streamConcatenation :: StreamConcatenation
    } deriving (Show, Generic)

data StreamExpression = StreamExpression {
    expression :: Expression,
    arrayRangeExpression :: Maybe ArrayRangeExpression
} deriving (Show, Generic)

data SliceSize =
    SSSimpleType {
        simpleType :: SimpleType
    } | SSConstantExpression {
        constantExpression :: ConstantExpression
    } deriving (Show, Generic)

data StreamOperator =
    SOGreater | SOLesser deriving (Show, Generic)

data StreamConcatenation =
    StreamConcatenation {
        streamExpressions :: [StreamExpression]
    } deriving (Show, Generic)

data ArrayRangeExpression =
    ARE { expression :: Expression }
    | AREColon { start :: Expression, end :: Expression}
    | AREPlusColon { start :: Expression, end :: Expression}
    | AREMinusColon { start :: Expression, end :: Expression} deriving (Show, Generic)

---- 8.2 - Subroutine Calls ---
---- 8.3 - Expressions ----

-- | Incomplete production rule
data IncOrDecExpression = IncOrDecExpression deriving (Show, Generic)
-- | Incomplete production rule
data ConditionalExpression = ConditionalExpressions deriving (Show, Generic)
-- | Incomplete production rule
data ConstantExpression =
    CEPrimary {
        constantPrimary :: ConstantPrimary
    } deriving (Show, Generic)
-- | Incomplete production rule
data ConstantMintypmaxExpression =
    CMESingle ConstantExpression
    | CMETriple ConstantExpression ConstantExpression ConstantExpression deriving (Show, Generic)

-- | Incomplete production rule
data ConstantParamExpression =
    CPEMintypmax {
        constantMintypmaxExpression :: ConstantMintypmaxExpression
    } | CPEDataType {
        dataType :: DataType
    } | CPEDollar deriving (Show, Generic)

-- | Incomplete production rule
data ParamExpression =
    PEMintypmax {
        mintypmaxExpression :: MintypmaxExpression
    } | PEDataType {
        dataType :: DataType
    } | PEDollar deriving (Show, Generic)
-- | Incomplete production rule
data ConstantRangeExpression = ConstantRangeExpression deriving (Show, Generic)
-- | Incomplete production rule
data ConstantPartSelectRange = ConstantPartSelectRange deriving (Show, Generic)
-- | Incomplete production rule
data ConstantRange = CRExpression {
    start :: ConstantExpression,
    end :: ConstantExpression
}deriving (Show, Generic)
-- | Incomplete production rule
data ConstantIndexedRange = ConstantIndexedRange deriving (Show, Generic)
-- | Incomplete production rule
data Expression =
    EPrimary {
        primary :: Primary
    } deriving (Show, Generic)
-- | Incomplete production rule
data TaggedUnionExpression = TaggedUnionExpression deriving (Show, Generic)
-- | Incomplete production rule
data InsideExpression = InsideExpression deriving (Show, Generic)
-- | Incomplete production rule
data ValueRange = ValueRange deriving (Show, Generic)
-- | Incomplete production rule
data MintypmaxExpression =
    MESingle {
        expression :: Expression
    } | METripple {
        start :: Expression,
        mid :: Expression,
        end :: Expression
    }deriving (Show, Generic)
-- | Incomplete production rule
data ModulePathConditionalExpression = ModulePathConditionalExpression deriving (Show, Generic)
-- | Incomplete production rule
data ModulePathExpression = ModulePathExpression deriving (Show, Generic)
-- | Incomplete production rule
data ModulePathMintypmaxExpression = ModulePathMintypmaxExpression deriving (Show, Generic)
-- | Incomplete production rule
data PartSelectRange = PSRConstant { constantRange :: ConstantRange } | PSRIndexed {indexedRange :: IndexedRange } deriving (Show, Generic)
-- | Incomplete production rule
data IndexedRange = IRPlusColon {expression :: Expression, constantExpression :: ConstantExpression} | IRMinusColon {expression :: Expression, constantExpression :: ConstantExpression} deriving (Show, Generic)

data GenvarExpression = GenvarExpression { constantExpression :: ConstantExpression} deriving (Show, Generic)
---- 8.4 - Primaries ----
-- | Incomplete production rule
data ConstantPrimary =
    CPLiteral {
        primaryLiteral :: PrimaryLiteral
    } deriving (Show, Generic)

data ImplicitClassHandle = ICHThisSuper | ICHSuper | ICHThis deriving (Show, Generic)
data BitSelect =
    BitSelect {
        expressions :: [Expression]
    } deriving (Show, Generic)

data Select =
    Select {
        memberIdentifierBitSelects :: MemberIdentifierBitSelects,
        partSelectRange :: Maybe PartSelectRange
    } | SelectAlt { bitSelectsOrPartSelectRanges :: [Either Expression PartSelectRange] }deriving (Show, Generic)

data MemberIdentifierBitSelects = MIBSList { list :: [(MemberIdentifier, BitSelect)]} | MIBSSingleton { singleton :: BitSelect } deriving (Show, Generic)

data ConstantBitSelect =
    ConstantBitSelect {
        constantExpressions :: [ConstantExpression]
    } deriving (Show, Generic)
data ConstantSelect = ConstantSelect deriving (Show, Generic)

data Primary =
    PLiteral {
        primaryLiteral :: PrimaryLiteral
    } | PHierarchicalIdentifier {
        hierarchicalIdentifier :: HierarchicalIdentifier,
        select :: Select
     } deriving (Show, Generic)

-- | Incomplete production rule
data PrimaryLiteral = PLNumber { number :: Number } deriving (Show, Generic)
data TimeLiteral =  TLUnsigned UnsignedNumber TimeUnit
                    | TLFixedPoint UnsignedNumber UnsignedNumber TimeUnit deriving (Show, Generic)

data TimeUnit = Second | Millisecond | Microsecond | Nanosecond | Picosecond | Femtosecond deriving (Eq, Show, Generic)

---- 8.5 - Expression Left-Side Values ----
-- | Incomplete production rule
data NetLValue =
    NetLValue {
            psOrHierachicalNetIdentifier :: PsOrHierachicalNetIdentifier,
            constantSelect               :: ConstantSelect
    } deriving (Show, Generic)

data VariableLvalue = VLHierarchical {
    implicitClassHandleOrPackageScope :: Maybe ImplicitClassHandleOrPackageScope,
    hierarchicalVariableIdentifier :: HierarchicalVariableIdentifier,
    select :: Select
} | VLList {
    variableLvalues :: [VariableLvalue]
} | VLAssignmentPattern {
    assignmentPatternExpressionType :: Maybe AssignmentPatternExpressionType,
    assignmentPatternVariableLvalue :: AssignmentPatternVariableLvalue
} | VLStreamingConcatenation {
    streamingConcatenation :: StreamingConcatenation
} deriving (Show, Generic)

type ImplicitClassHandleOrPackageScope = Either ImplicitClassHandle PackageScope
---- 8.6 - Operators ----
---- 8.7 - Numbers ----

-- | Incomplete production rule
data Number =
    NIntegral {
        integralNumber :: IntegralNumber
    }
    -- | NReal {
    --     realNumber :: RealNumber
    -- }
    deriving (Show, Generic)

-- | Incomplete production rule
data IntegralNumber =
    INDecimal {
        decimalNumber :: DecimalNumber
    } deriving (Show, Generic)

-- | Incomplete production rule
data DecimalNumber =
    DNUnsigned {
        unsignedNumber :: UnsignedNumber
    } deriving (Show, Generic)
type UnsignedNumber = Word

---- 8.8 - Strings ----

---- Sec 9 ----
---- 9.1 - Attributes ----
-- | Incomplete production rule
data AttributeInstance =
    AttributeInstance{
        attrSpec :: [AttrSpec]
    }   deriving (Show, Generic)

-- | Incorrect production rule (Constant Expression should not be Maybe Identifier)
data AttrSpec =
    AttrSpec{
        attrName           :: AttrName,
        constantExpression :: Maybe Identifier
    }   deriving (Show, Generic)

newtype AttrName = AttrName Identifier deriving (Show, Generic)
---- 9.2 - Comments ----
---- 9.3 - Identifiers ----
--- Identifier may change to be more complex/not lazy
type Identifier = String
newtype BlockIdentifier = BlockIdentifier Identifier deriving (Show, Generic)
newtype ClassIdentifier = ClassIdentifier Identifier deriving (Show, Generic)
data HierarchicalIdentifier = HierarchicalIdentifier {
    root :: Bool,
    hierarchy :: [(Identifier, ConstantBitSelect)],
    identifier :: Identifier
} deriving (Show, Generic)
newtype HierarchicalVariableIdentifier = HierarchicalVariableIdentifier HierarchicalIdentifier deriving (Show, Generic)
newtype InterfaceIdentifier = InterfaceIdentifier Identifier deriving (Show, Generic)
newtype ParameterIdentifier = ParameterIdentifier Identifier deriving (Show, Generic)
newtype PortIdentifier = PortIdentifier Identifier deriving (Show, Generic)
newtype ModuleIdentifier = ModuleIdentifier Identifier deriving (Show, Generic)
newtype MemberIdentifier = MemberIdentifier Identifier deriving (Show, Generic)
newtype ModportIdentifier = ModportIdentifier Identifier deriving (Show, Generic)
newtype PackageIdentifier = PackageIdentifier Identifier deriving (Show, Generic)
data PackageScope =
    PSIdentifier PackageIdentifier
    | PSUnit deriving (Show, Generic)
data PsClassIdentifier =
    PsClassIdentifier {
        packageScope :: Maybe PackageScope,
        classIdentifier :: ClassIdentifier
    } deriving (Show, Generic)

data PsOrHierachicalNetIdentifier =
    POHNINet {
        maybePackageScope :: Maybe PackageScope,
        netIdentifier     :: NetIdentifier
    } | POHNIHierachicalNet {

    } deriving (Show, Generic)

-- | Incomplete production rule
data PsParameterIdentifier =
    PPIScoped {
        packageOrClassScope :: Maybe (Either PackageScope ClassScope),
        parameterIdentifier :: ParameterIdentifier
    } deriving (Show, Generic)

data PsTypeIdentifier =
    PsTypeIdentifier {
        localOrPackageScope :: Maybe LocalOrPackageScope,
        typeIdentifier :: TypeIdentifier
    } deriving (Show, Generic)

data LocalOrPackageScope =
    LOPSLocal | LOPSPackageScope { packageScope :: PackageScope } deriving (Show, Generic)

newtype NetIdentifier = NetIdentifier Identifier deriving (Show, Generic)

newtype TypeIdentifier = TypeIdentifier Identifier deriving (Show, Generic)
newtype VariableIdentifier = VariableIdentifier Identifier deriving (Show, Generic)
-- | Incorrect
---- 9.4 - White space ----
