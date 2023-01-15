module V2H.Ast where

data AstRoot = AstLibrary LibraryText | AstSource SourceText

data LibraryText = LibraryText

data SourceText = SourceText {
    timeunitsDeclarations :: Maybe TimeunitsDeclarations,
    description           :: [Description]
}

data ModuleDeclaration =
    MDNonAnsiHeader {
        moduleNonAnsiHeader :: ModuleNonAnsiHeader,
        timeunitsDeclarations :: Maybe TimeunitsDeclarations,
        moduleItems :: [ModuleItem]
    } | MDAnsiHeader {
        moduleAnsiHeader :: ModuleAnsiHeader,
        timeunitsDeclarations :: Maybe TimeunitsDeclarations,
        nonPortModuleItems :: [NonPortModuleItem]
    } | MDAttributes {
        attributeInstances :: [AttributeInstance],
        lifetime :: Maybe Lifetime,
        moduleIdentifier :: ModuleIdentifier,
        timeunitsDeclaration :: Maybe TimeunitsDeclarations,
        moduleItems :: [ModuleItem]
    } | MDNonAnsiExtern {
        moduleNonAnsiHeader :: ModuleNonAnsiHeader
    } | MDAnsiExtern {
        moduleAnsiHeader :: ModuleAnsiHeader
    }

data TimeunitsDeclarations = TimeunitsDeclarations
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
    }

data ModuleNonAnsiHeader = ModuleNonAnsiHeader
data ModuleItem =
    MIPortDeclaration {
        portDeclaration :: PortDeclaration
    } | MINonPortModuleItem {
        nonPortModuleItem :: NonPortModuleItem
    }

data PortDeclaration =
    PDInoutDeclaration {
        attributeInstances :: [AttributeInstance],
        inoutDeclaration :: InoutDeclaration
    } | PDInputDeclaration {
        attributeInstances :: [AttributeInstance],
        inputDeclaration :: InputDeclaration
    } | PDOutputDeclaration {
        attributeInstances :: [AttributeInstance],
        outputDeclaration :: OutputDeclaration
    } | PDRefDeclaration {
        attributeInstances :: [AttributeInstance],
        refDeclaration :: RefDeclaration
    } | PDInterfacePortDeclaration {
        attributeInstances :: [AttributeInstance],
        interfacePortDeclaration :: InterfacePortDeclaration
    }

data InoutDeclaration =
    InoutDeclaration {
        inoutnetPortType :: InoutnetPortType,
        portIdentifiers :: [PortIdentifier]
    }

data PortIdentifier =
    PortIdentifier {
        name :: Identifier,
        unpackedDimension :: [UnpackedDimension]
    }

data UnpackedDimension =
    UDConstantRange {
        constantRange :: ConstantRange
    } | UDConstantExpression {
        constantExpression :: ConstantExpression
    }

data ConstantRange =
    CR{
        beg :: ConstantExpression,
        end :: ConstantExpression
    }

data ConstantExpression =
    ConstantExpression{
        constantPrimary :: ConstantPrimary
    }

data PrimaryLiteral = PrimaryLiteral
data ModuleAnsiHeader = ModuleAnsiHeader
data AttributeInstance = AttributeInstance
data Lifetime = Lifetime
data ModuleIdentifier = ModuleIdentifier
data GenerateRegion = GenerateRegion
data ClassQualifier = ClassQualifier

-- | Incomplete production rule
data NonPortModuleItem =
    NPMIGenerateRegion {
        generateRegion :: GenerateRegion
    } | NPMIModuleOrGenerateItem {
        moduleOrGenerateItem :: ModuleOrGenerateItem
    }

-- | Incomplete production rule
data ModuleOrGenerateItem =
    MOGIModuleCommonItem{
        attributeInstances :: [AttributeInstance],
        moduleCommonItem :: ModuleCommonItem
    }

-- | Incomplete production rule
data ModuleCommonItem =
    MCIModuleOrGenerateItemDeclaration {
        moduleOrGenerateItem :: ModuleOrGenerateItem
    } | MCIAlwaysConstruct {
        alwaysConstruct :: AlwaysConstruct
    }

data AlwaysConstruct =
    AlwaysConstruct {alwaysKeyword :: AlwaysKeyword, statement :: Statement}

data AlwaysKeyword = Always | AlwaysComb | AlwaysLatch | AlwaysFF

data Statement =
    Statement {
        blockIdentifier :: Maybe BlockIdentifier,
        attributeInstances :: [AttributeInstance],
        statementItem :: StatementItem
    }

-- | Incomplete production rule
data StatementItem =
    SIBlockingAssignment {
        blockingAssignment :: BlockingAssignment
    } | SINonblockingAssignment {
        nonblockingAssignment :: NonBlockingAssignment
    }

-- | Incomplete production rule
data BlockingAssignment =
    BAOperatorAssignment {
        operatorAssignment :: OperatorAssignment
    }

data OperatorAssignment  =
    OperatorAssignment {
        variableLvalue :: VariableLvalue,
        assignmentOperator :: AssignmentOperator,
        expression :: Expression
    }

data Expression =
    EPrimary {
        primary :: Primary
    } | EUnaryOperator {
        unaryOperator :: UnaryOperator,
        attributeInstances :: [AttributeInstance],
        primary :: Primary
    } | EIncDec {
        incOrDecExpression :: IncOrDecExpression
    } | EOperatorAssignment {
        operatorAssignment :: OperatorAssignment
    } | EBinaryOperator {
        subExpressions :: (Expression,Expression),
        binaryOperator :: BinaryOperator,
        attributeInstances :: [AttributeInstance] ,
        conditionalExpression :: ConditionalExpression
    } | EInsideExpression {
        insideExpression :: InsideExpression
    } | ETaggedUnionExpression {
        taggedUnionExpression :: TaggedUnionExpression
    }

data Primary =
    PPrimaryLiteral { primaryLiteral :: PrimaryLiteral }
    | PPrimaryHierarchicalIdentifier {
        scopeQualifier :: Maybe ScopeQualifier,
        hierarchicalIdentifier :: HierarchicalIdentifier,
        select :: Select
    } | PEmptyQueue
    | PConcatenation {
        rangeExpression :: Maybe RangeExpression
    } | PMultipleConcatenation {
        rangeExpression :: Maybe RangeExpression
    } | PFunctionSubroutineCall {
        functionSubRoutineCall :: FunctionSubroutineCall
    } | PLetExpression {
        letExpression :: LetExpression
    } | PMintypmaxExpression {
        mintypemaxExpression :: MinTypMaxExpression
    } | PCast {
        cast :: Cast
    } | PAssignmentPatternExpression {
        assignmentPatternExpression :: AssignmentPatternExpression
    } | PStreamingConcatenation | PSequenceMethodCall | PThis | PDollar | PNull

data MinTypMaxExpression =
    MTMESingleExpression {
        expression :: Expression
    } | MTMETrippleExpression {
        expressionA :: Expression,
        expressionB :: Expression,
        expressionC :: Expression
    }

data ScopeQualifier =
    SQClassQualifier {
        classQualifier :: ClassQualifier
    }
    | SQPackageScope { packageScope :: PackageScope }

data AssignmentOperator =
    Equals
    | PlusEquals
    | MinusEquals
    | ProductEquals
    | DivideEquals
    | PercentageEquals
    | AmpersandEquals
    | PipeEquals
    | CaretEquals
    | ShiftLeftEqals
    | ShiftRightEquals
    | ArithmeticShiftLeftEquals
    | ArithmeticShiftRightEquals