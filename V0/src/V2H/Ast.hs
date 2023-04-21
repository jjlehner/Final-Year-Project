module V2H.Ast where

----- Sec 1 -----
---- 1.1 - Library Source Text ----
-- | Incomplete production rule
data LibraryText = LibraryText deriving (Show)

-- | Incomplete production rule
data LibraryDescription = LibraryDescription deriving (Show)

-- | Incomplete production rule
data LibraryDeclaration = LibraryDeclaration deriving (Show)

-- | Incomplete production rule
data IncludeStatement = IncludeStatement deriving (Show)

---- 1.2 - SystemVerilog Source Text ----

data SourceText = SourceText {
    timeunitsDeclaration :: Maybe TimeunitsDeclaration,
    description           :: [Description]
} deriving (Show)

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
        packageItem :: PackageItem
    } | DBindDirective {
        attributeInstances :: [AttributeInstance],
        bindDirective :: BindDirective
    } | DConfigDeclaration {
        configDeclaration :: ConfigDeclaration
    } deriving (Show)

data ModuleNonAnsiHeader =
    ModuleNonAnsiHeader {
        attributeInstances :: [AttributeInstance],
        moduleKeyword :: ModuleKeyword,
        lifetime :: Maybe Lifetime,
        moduleIdentifier :: ModuleIdentifier,
        packageImportDeclarations :: [PackageImportDeclaration],
        parameterPorts :: Maybe ParameterPorts,
        ports :: [Port]
    } deriving (Show)

data ModuleAnsiHeader =
    ModuleAnsiHeader {
        attributeInstances :: [AttributeInstance],
        moduleKeyword :: ModuleKeyword,
        lifetime :: Maybe Lifetime,
        moduleIdentifier :: ModuleIdentifier,
        packageImportDeclarations :: [PackageImportDeclaration],
        parameterPorts :: Maybe ParameterPorts,
        portDeclarations :: Maybe PortDeclarations
    } deriving (Show)

data ModuleDeclaration =
    MDNonAnsiHeader {
        moduleNonAnsiHeader :: ModuleNonAnsiHeader,
        timeunitsDeclarations :: Maybe TimeunitsDeclaration,
        moduleItems :: [ModuleItem]
    } | MDAnsiHeader {
        moduleAnsiHeader :: ModuleAnsiHeader,
        timeunitsDeclarations :: Maybe TimeunitsDeclaration,
        nonPortModuleItems :: [NonPortModuleItem]
    } | MDAttributes {
        attributeInstances :: [AttributeInstance],
        lifetime :: Maybe Lifetime,
        moduleIdentifier :: ModuleIdentifier,
        timeunitsDeclaration :: Maybe TimeunitsDeclaration,
        moduleItems :: [ModuleItem]
    } | MDNonAnsiExtern {
        moduleNonAnsiHeader :: ModuleNonAnsiHeader
    } | MDAnsiExtern {
        moduleAnsiHeader :: ModuleAnsiHeader
    } deriving (Show)

data ModuleKeyword = Module | Macromodule deriving (Show)

-- | Incomplete production rule
data InterfaceDeclaration = InterfaceDeclaration deriving (Show)

-- | Incomplete production rule
data InterfaceNonAnsiHeader = InterfaceNonAnsiHeader deriving (Show)

-- | Incomplete production rule
data InterfaceAnsiHeader = InterfaceAnsiHeader deriving (Show)

-- | Incomplete production rule
data ProgramDeclaration = ProgramDeclaration deriving (Show)

-- | Incomplete production rule
data ProgramNonAnsiHeader = ProgramNonAnsiHeader deriving (Show)

-- | Incomplete production rule
data ProgramAnsiHeader = ProgramAnsiHeader deriving (Show)

-- | Incomplete production rule
data CheckerDeclaration = CheckerDeclaration deriving (Show)

-- | Incomplete production rule
data ClassDeclaration = ClassDeclaration deriving (Show)

-- | Incomplete production rule
data InterfaceClassType = InterfaceClassType deriving (Show)

-- | Incomplete production rule
data InterfaceClassDeclaration = InterfaceClassDeclaration deriving (Show)

-- | Incomplete production rule
data InterfaceClassItem = InterfaceClassItem deriving (Show)

-- | Incomplete production rule
data InterfaceClassMethod = InterfaceClassMethod deriving (Show)

-- | Incomplete production rule
data PackageDeclaration = PackageDeclaration deriving (Show)

-- | Incomplete production rule
data TimeunitsDeclaration = TimeunitsDeclaration {} deriving (Show)

---- 1.3 - Module parameters and Ports ----
data ParameterPorts =
    ParameterPorts {
        parameterAssignments :: [ParamAssignment],
        parameterPortDeclaration :: [ParameterPortDeclaration]
    } deriving (Show)

-- | Incomplete production rule
data ParameterPortDeclaration =
    PPD{
        parameterDeclaration :: ParameterDeclaration
    } | PPDLocal {
        localParameterDeclaration :: LocalParameterDeclaration
    } | PPDDataTypeParamAssignments {
        dataType :: DataType,
        parameterAssignments :: [ParamAssignment]
    } | PPDTypeAssignments {
        typeAssignments :: [TypeAssignment]
    } deriving (Show)

-- | Incomplete production rule
-- Referred to as list_of_port_declarations in System Verilog Official Grammar
type PortDeclarations = [PortDeclarationsItem]
data PortDeclarationsItem =
    PortDeclarationsItem {
        attributeInstances :: [AttributeInstance],
        ansiPortDeclaration :: AnsiPortDeclaration
    } deriving (Show)

-- | Incomplete production rule
data PortDeclaration =
    PDInputDeclaration {
        attributeInstances :: [AttributeInstance],
        inputDeclaration :: InputDeclaration
    } deriving (Show)

data Port =
    PUnamed {
        portExpression :: Maybe PortExpression
    } | PNamed {
        portIdentifier :: PortIdentifier,
        portExpression :: Maybe PortExpression
    } deriving (Show)

data PortExpression =
    PortExpression { portReferences :: [PortReference] } deriving (Show)

data PortReference =
    PortReference {
        portIdentifier :: PortIdentifier,
        constantSelect :: ConstantSelect
    } deriving (Show)

data PortDirection = PDInput | PDOutput | PDInout | PDRef deriving (Show)

data NetPortHeader = NetPortHeader {
        portDirection :: Maybe PortDirection,
        netPortType :: NetPortType
    } deriving (Show)

data VariablePortHeader = VariablePortHeader {
        portDirection :: Maybe PortDirection,
        variablePortType :: VariablePortType
    } deriving (Show)

-- | Incomplete production rule
data InterfacePortHeader =
    IPDNamed {
        interfaceIdentifier :: InterfaceIdentifier,
        modportIdentifier :: Maybe ModportIdentifier
    } | IPDAnonymous {
        modportIdentifier :: Maybe ModportIdentifier
    } deriving (Show)

-- | Incomplete production rule
data AnsiPortDeclaration =
    APDNetOrInterfaceHeader {
        netOrInterfacePortHeader :: Maybe (Either NetPortHeader InterfacePortHeader),
        portIdentifier :: PortIdentifier,
        unpackedDimensions :: [UnpackedDimension],
        constantExpression :: Maybe ConstantExpression
    } | APDVariableHeader {
        variablePortHeader :: Maybe VariablePortHeader,
        portIdentifier :: PortIdentifier,
        variableDimensions :: [VariableDimension],
        constantExpression :: Maybe ConstantExpression
    } | APDDirection {
        portDirection :: Maybe PortDirection,
        portIdentifier :: PortIdentifier,
        expression :: Maybe Expression
    } deriving (Show)
---- 1.4 - Module Items ----
-- | Incomplete production rule
data BindDirective = BindDirective deriving (Show)

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
    } deriving (Show)

data ModuleItem =
    MIPort_Declaration {
        portDeclaration :: PortDeclaration
    } | MINonPortModuleItem {
        nonPortModuleItem :: NonPortModuleItem
    } deriving (Show)

-- | Incomplete production rule
data ModuleOrGenerateItem = MOGIModuleCommonItem{
        attributeInstances :: [AttributeInstance],
        moduleCommonItem :: ModuleCommonItem
    } deriving (Show)

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
    } deriving (Show)

-- | Incomplete production rule
data ModuleOrGenerateItemDeclaration = ModuleOrGenerateItemDeclaration deriving (Show)

-- | Incomplete production rule
data ElaborationSystemTask = ElaborationSystemTask deriving (Show)

---- 1.5 - Configuration Source Text ----
-- | Incomplete production rule
data ConfigDeclaration = ConfigDeclaration deriving (Show)

---- 1.6 - Interface Items ----
---- 1.7 - Program Items ----
---- 1.8 - Checker Items ----
---- 1.9 - Class Items ----
---- 1.10 - Constraints ----
---- 1.11 - Package Items ----
-- | Incomplete production rule
data PackageItem = PackageItem deriving (Show)
----- Sec 2 -----
---- 2.1.1 - Module Parameter Declarations ----
-- | Incomplete production rule
data LocalParameterDeclaration = LPDParamAssignments {
        dataTypeOrImplicit :: DataTypeOrImplicit,
        paramAssignments :: [ParamAssignment]
    } | LPDTypeAssignments {
        typeAssignments :: [TypeAssignment]
    } deriving (Show)

data ParameterDeclaration =
    PDDataTypeOrImplicit {
        dataTypeOrImplicit :: DataTypeOrImplicit,
        paramAssignments :: [ParamAssignment]
    }
    | PDTypeAssignments {
        typeAssignments :: [TypeAssignment]
    } deriving (Show)

-- | Incomplete production rule
data SpecparamDeclaration = SpecparamDeclaration deriving (Show)

---- 2.1.2 - Port Declarations ----

data InputDeclaration =
    IDNetIdentifiers {
        netPortType :: NetPortType,
        portIdentifiers :: [PortIdentifier]
    } | IDVariableIdentifiers {
        variablePortType :: VariablePortType,
        variableIdentifiers :: [VariableIdentifier]
    } deriving (Show)

---- 2.1.3 - Type Declarations ----
data PackageImportItem =
    PIIIdentifier {
        packageIdentifier :: PackageIdentifier,
        identifier :: Identifier
    } | PIIWildcard {
        packageIdentifier :: PackageIdentifier
    } deriving (Show)

data Lifetime = Static | Automatic deriving (Show)

-- | Incomplete production rule
data PackageImportDeclaration = PackageImportDeclaration [PackageImportItem] deriving (Show)
---- 2.2.1 - Net and Variable Types ----
data DataType = DTIntegerVector {
        integerVectorType :: IntegerVectorType,
        signing :: Maybe Signing,
        packedDimension :: [PackedDimension]
    } deriving (Show)

type DataTypeOrImplicit = Either DataType ImplicitDataType
data ImplicitDataType =
    ImplicitDataType {
        signing :: Maybe Signing,
        packedDimensions :: [PackedDimension]
    } deriving (Show)

data IntegerVectorType = Bit | Logic | Reg deriving (Show)

data NetType = Supply0 | Supply1 | Tri | Triand | Trior | Trireg| Tri0 | Tri1 | Uwire| Wire | Wand | Wor deriving (Show)

-- | Incomplete production rule
data NetPortType =
    NPTDataOrImplicit {
        netType :: Maybe NetType,
        dataTypeOrImplicit :: DataTypeOrImplicit
    } deriving (Show)

data VariablePortType = VariablePortType VarDataType deriving (Show)
data VarDataType =
    VDT { dataType :: DataType }
    | VDTOrImplicit { dataTypeOrImplicit :: DataTypeOrImplicit } deriving (Show)

data Signing = SSigned | SUnsigned deriving (Show)

---- 2.2.2 - Strengths ----
data DriveStrength = DriveStrength deriving (Show)

---- 2.2.3 - Delays ----
---- 2.3 - Declarations Lists ----
---- 2.4 - Declarations Assignments ----
-- | Incomplete production rule
data DefparamAssignment = DefparamAssignment deriving (Show)

-- | Incomplete production rule
data ParamAssignment = ParamAssignment {
        parameterIdentifier :: ParameterIdentifier,
        unpackedDimensions :: [UnpackedDimension],
        constantParamExpression :: Maybe ConstantParamExpression
    } deriving (Show)

-- | Incomplete production rule
data SpecparamAssignment = SpecparamAssignment deriving (Show)

-- | Incomplete production rule
data TypeAssignment =
    TypeAssignment {
        identifier :: TypeIdentifier,
        dataType :: Maybe DataType
    } deriving (Show)

-- | Incomplete production rule
data PulseControlSpecparam = PulseControlSpecparam deriving (Show)

-- | Incomplete production rule
data ErrorLimitValue = ErrorLimitValue deriving (Show)

-- | Incomplete production rule
data LimitValue = LimitValue deriving (Show)

-- | Incomplete production rule
data VariableDeclAssignment = VariableDeclAssignment deriving (Show)

-- | Incomplete production rule
data ClassNew = ClassNew deriving (Show)

-- | Incomplete production rule
data DynamicArrayNew = DynamicArrayNew deriving (Show)

---- 2.5 - Declaration Ranges ----
data UnpackedDimension =
    UDConstantRange {
        constantRange :: ConstantRange
    } | UDConstantExpression {
        constantExpression :: ConstantExpression
    } deriving (Show)

data PackedDimension =
    PDConstantRange {
        constantRange :: ConstantRange
    } | PDUnsized {
        unsizedDimension :: UnsizedDimension
    } deriving (Show)
-- | Incomplete production rule
data VariableDimension = VDUnsized UnsizedDimension deriving (Show)
data UnsizedDimension = UnsizedDimension deriving (Show)
---- 2.6 - Function Declarations ----
---- 2.7 - Task Declarations ----
---- 2.8 - Block Item Declarations ----
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
---- 4.1.2 - Interface Instantiation ----
data InterfaceInstantiation = InterfaceInstantiation deriving (Show)
---- 4.1.3 - Program Instantiation ----
data ProgramInstantiation = ProgramInstantiation deriving (Show)
---- 4.1.4 - Checker Instantiation ----
---- 4.2 - Generated Instantiation ----
data GenerateRegion = GenerateRegion {
    generateIterms :: [GenerateItem]
} deriving (Show)

-- | Incomplete production rule
data LoopGenerateConstruct = LoopGenerateConstruct deriving (Show)

-- | Incomplete production rule
data ConditionalGenerateConstruct = ConditionalGenerateConstruct deriving (Show)

-- | Incomplete production rule
data GenerateItem = GenerateItem deriving (Show)

---- Sec 5 ----
---- 5.1 - UDP Declaration ----
-- | Incomplete production rule
data UdpNonAnsiDeclaration = UdpNonAnsiDeclaration deriving (Show)

-- | Incomplete production rule
data UdpAnsiDeclaration = UdpAnsiDeclaration deriving (Show)

-- | Incomplete production rule
data UdpDeclaration = UdpDeclaration deriving (Show)
---- 5.2 - UDP Ports ----
---- 5.3 - UDP Body ----
---- 5.4 - UDP Instantiation ----

---- Sec 6 -----
---- 6.1 - Continuous Assignment And Net Alias Statements ----
data ContinuousAssign = ContinuousAssign deriving (Show)

-- | Incomplete production rule
data NetAlias = NetAlias deriving (Show)

-- | Incomplete production rule
data NetAssignment = NetAssignment deriving (Show)
---- 6.2 - Procedural Blocks And Assignments ----
-- | Incomplete production rule
data InitialConstruct = InitialConstruct deriving (Show)

-- | Incomplete production rule
data AlwaysConstruct =
    AlwaysConstruct {
        alwaysKeyword :: AlwaysKeyword,
        statement :: Statement
    } deriving (Show)

-- | Incomplete production rule
data FinalConstruct = FinalConstruct deriving (Show)

data AlwaysKeyword = Always | AlwaysComb | AlwaysLatch | AlwaysFF deriving (Show)

-- | Incomplete production rule
data BlockingAssignment = BlockingAssignment deriving (Show)

-- | Incomplete production rule
data OperatorAssignment = OperatorAssignment deriving (Show)

-- | Incomplete production rule
data AssignmentOperator = AssignmentOperator deriving (Show)

-- | Incomplete production rule
data NonblockingAssignment = NonblockingAssignment deriving (Show)

-- | Incomplete production rule
data ProceduralContinuousAssignment = ProceduralContinuousAssignment deriving (Show)

-- | Incomplete production rule
data VariableAssignment = VariableAssignment deriving (Show)
---- 6.3 - Parallel And Sequential Blocks ----
---- 6.4 - Statements ----
-- | Incomplete production rule
data StatementOrNull = StatementOrNull deriving (Show)

-- | Incomplete production rule
data Statement = Statement deriving (Show)

-- | Incomplete production rule
data StatementItem = StatementItem deriving (Show)

-- | Incomplete production rule
data FunctionStatement = FunctionStatement deriving (Show)

-- | Incomplete production rule
data FunctionStatementOrNull = FunctionStatementOrNull deriving (Show)

-- | Incomplete production rule
data VariableIdentifierList = VariableIndentifierList deriving (Show)
---- 6.5 - Timing Control Statements ----
---- 6.6 - Conditional Statements ----
---- 6.7 - Case Statements ----
---- 6.7.1 - Patterns ----
---- 6.8 - Looping Statements ----
---- 6.9 - Subroutine Call Statements ----
---- 6.10 - Assertion Statements ----
-- | Incomplete production rule
data AssertionItem = AssertionItem deriving (Show)
---- 6.11 - Clocking Block ----
---- 6.12 - Randsequence ----

---- Sec 7 ----
---- 7.1 - Specify Block Declaration ----
data SpecifyBlock = SpecifyBlock deriving (Show)
---- 7.2 - Specify Path Declarations ----
---- 7.3 - Specify Block Terminals ----
---- 7.4 - Specify Path Delays ----
---- 7.5 - System Timing Checks ----
---- 7.5.2 - System Timing Check Command Arguments ----
---- 7.5.3 - System Timing Check Event Definitions ----

---- Sec 8 ----
---- 8.1 - Concatenations ----
---- 8.2 - Subroutine Calls ---
---- 8.3 - Expressions ----

-- | Incomplete production rule
data IncOrDecExpression = IncOrDecExpression deriving (Show)
-- | Incomplete production rule
data ConditionalExpression = ConditionalExpressions deriving (Show)
-- | Incomplete production rule
data ConstantExpression =
    CEPrimary {
        constantPrimary :: ConstantPrimary
    } deriving (Show)
-- | Incomplete production rule
data ConstantMintypmaxExpression =
    CMESingle ConstantExpression
    | CMETriple ConstantExpression ConstantExpression ConstantExpression deriving (Show)

-- | Incomplete production rule
data ConstantParamExpression =
    CPEMintypmax {
        constantMintypmaxExpression :: ConstantMintypmaxExpression
    } | CPEDataType {
        dataType :: DataType
    } | CPEDollar deriving (Show)

-- | Incomplete production rule
data ParamExpression = ParamExpression deriving (Show)
-- | Incomplete production rule
data ConstantRangeExpression = ConstantRangeExpression deriving (Show)
-- | Incomplete production rule
data ConstantPartSelectRange = ConstantPartSelectRange deriving (Show)
-- | Incomplete production rule
data ConstantRange = CRExpression {constantExpression :: ConstantExpression }deriving (Show)
-- | Incomplete production rule
data ConstantIndexedRange = ConstantIndexedRange deriving (Show)
-- | Incomplete production rule
data Expression = Expression deriving (Show)
-- | Incomplete production rule
data TaggedUnionExpression = TaggedUnionExpression deriving (Show)
-- | Incomplete production rule
data InsideExpression = InsideExpression deriving (Show)
-- | Incomplete production rule
data ValueRange = ValueRange deriving (Show)
-- | Incomplete production rule
data MintypmaxExpression = MintypmaxExpression deriving (Show)
-- | Incomplete production rule
data ModulePathConditionalExpression = ModulePathConditionalExpression deriving (Show)
-- | Incomplete production rule
data ModulePathExpression = ModulePathExpression deriving (Show)
-- | Incomplete production rule
data ModulePathMintypmaxExpression = ModulePathMintypmaxExpression deriving (Show)
-- | Incomplete production rule
data PartSelectRange = PartSelectRange deriving (Show)
-- | Incomplete production rule
data IndexedRange = IndexedRanged deriving (Show)

data GenvarExpression = GenvarExpression ConstantExpression deriving (Show)
---- 8.4 - Primaries ----
-- | Incomplete production rule
data ConstantPrimary =
    CPLiteral {
        primaryLiteral :: PrimaryLiteral
    } deriving (Show)

data ConstantSelect = ConstantSelect deriving (Show)

-- | Incomplete production rule
data PrimaryLiteral = PLNumber { number :: Number } deriving (Show)
data TimeLiteral =  TLUnsigned UnsignedNumber TimeUnit
                    | TLFixedPoint UnsignedNumber UnsignedNumber TimeUnit

data TimeUnit = Second | Millisecond | Microsecond | Nanosecond | Picosecond | Femtosecond deriving (Eq, Show)

---- 8.5 - Expression Left-Side Values ----
-- | Incomplete production rule
data NetLValue =
    NetLValue {
            psOrHierachicalNetIdentifier :: PsOrHierachicalNetIdentifier,
            constantSelect :: ConstantSelect
    } deriving (Show)

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
    deriving (Show)

-- | Incomplete production rule
data IntegralNumber =
    INDecimal {
        decimalNumber :: DecimalNumber
    } deriving (Show)

-- | Incomplete production rule
data DecimalNumber =
    DNUnsigned {
        unsignedNumber :: UnsignedNumber
    } deriving (Show)
type UnsignedNumber = Word

---- 8.8 - Strings ----

---- Sec 9 ----
---- 9.1 - Attributes ----
-- | Incomplete production rule
data AttributeInstance =
    AttributeInstance{
        attrSpec :: [AttrSpec]
    }   deriving (Show)

-- | Incorrect production rule (Constant Expression should not be Maybe Identifier)
data AttrSpec =
    AttrSpec{
        attrName :: AttrName,
        constantExpression :: Maybe Identifier
    }   deriving (Show)

newtype AttrName = AttrName Identifier deriving (Show)
---- 9.2 - Comments ----
---- 9.3 - Identifiers ----
--- Identifier may change to be more complex/not lazy
type Identifier = String
newtype InterfaceIdentifier = InterfaceIdentifier Identifier deriving (Show)
newtype ParameterIdentifier = ParameterIdentifier Identifier deriving (Show)
newtype PortIdentifier = PortIdentifier Identifier deriving (Show)
newtype ModuleIdentifier = ModuleIdentifier Identifier deriving (Show)
newtype ModportIdentifier = ModportIdentifier Identifier deriving (Show)
newtype PackageIdentifier = PackageIdentifier Identifier deriving (Show)
data PackageScope =
    PSIdentifier PackageIdentifier
    | PSUnit deriving (Show)
data PsOrHierachicalNetIdentifier =
    POHNINet {
        maybePackageScope :: Maybe PackageScope,
        netIdentifier :: NetIdentifier
    } | POHNIHierachicalNet {

    } deriving (Show)

newtype NetIdentifier = NetIdentifier Identifier deriving (Show)

newtype TypeIdentifier = TypeIdentifier Identifier deriving (Show)
newtype VariableIdentifier = VariableIdentifier Identifier deriving (Show)
-- | Incorrect
---- 9.4 - White space ----