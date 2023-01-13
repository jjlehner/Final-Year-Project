module V2H.Ast where

data Ast = AstLibrary LibraryText | AstSource SourceText

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
data Description = Description
data ModuleNonAnsiHeader = ModuleNonAnsiHeader
data ModuleItem = ModuleItem
data ModuleAnsiHeader = ModuleAnsiHeader
data AttributeInstance = AttributeInstance
data Lifetime = Lifetime
data ModuleIdentifier = ModuleIdentifier

-- | Incomplete production rule
data NonPortModuleItem =
    NPMIModuleOrGenerateItem {
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
    } | PrimaryEmptyQueue
    | PrimaryConcatenation {
        rangeExpression :: Maybe RangeExpression
    } | PrimaryMultipleConcatenation {
        rangeExpression :: Maybe RangeExpression
    } | PrimaryFunctionSubroutineCall {
        functionSubRoutineCall :: FunctionSubroutineCall
    } | PrimaryLetExpression {
        letExpression :: LetExpression
    }



data ScopeQualifier =
    SQClassQualifier { classQualifier :: ClassQualifier },
    | SQPackageScope { packageScope :: PackageScope }