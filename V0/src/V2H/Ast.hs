module V2H.Ast (
    module V2H.Ast.Sec1.ConfigurationSourceText,
    module V2H.Ast.Sec1.LibrarySourceText,
    module V2H.Ast.Sec1.ModuleItems,
    module V2H.Ast.Sec1.ModuleParametersAndPorts,
    module V2H.Ast.Sec1.PackageItems,
    module V2H.Ast.Sec1.SourceText,
    module V2H.Ast.Sec2.DeclarationAssignments,
    module V2H.Ast.Sec2.TypeDeclarations,
    module V2H.Ast.Sec4.GeneratedInstantiation,
    module V2H.Ast.Sec4.InterfaceInstantiation,
    module V2H.Ast.Sec4.ProgramInstantiation,
    module V2H.Ast.Sec5.UdpDeclaration,
    module V2H.Ast.Sec6.AssertionStatements,
    module V2H.Ast.Sec6.ContinuousAssignmentAndNetAliasStatements,
    module V2H.Ast.Sec6.ProceduralBlocksAndAssignments,
    module V2H.Ast.Sec8.Primaries,
    module V2H.Ast.Sec9.Attributes,
    module V2H.Ast.Sec9.Identifiers,
    AstRoot (..)
) where

import V2H.Ast.Sec1.ConfigurationSourceText
import V2H.Ast.Sec1.LibrarySourceText
import V2H.Ast.Sec1.ModuleItems
import V2H.Ast.Sec1.ModuleParametersAndPorts
import V2H.Ast.Sec1.PackageItems
import V2H.Ast.Sec1.SourceText
import V2H.Ast.Sec2.DeclarationAssignments
import V2H.Ast.Sec2.TypeDeclarations
import V2H.Ast.Sec4.GeneratedInstantiation
import V2H.Ast.Sec4.InterfaceInstantiation
import V2H.Ast.Sec4.ProgramInstantiation
import V2H.Ast.Sec5.UdpDeclaration
import V2H.Ast.Sec6.AssertionStatements
import V2H.Ast.Sec6.ContinuousAssignmentAndNetAliasStatements
import V2H.Ast.Sec6.ProceduralBlocksAndAssignments
import V2H.Ast.Sec8.Primaries
import V2H.Ast.Sec9.Attributes
import V2H.Ast.Sec9.Identifiers

data AstRoot = ARLibrary LibraryText | ARSource SourceText

-- data LibraryText = LibraryText

-- data ModuleItem =
--     MIPortDeclaration {
--         portDeclaration :: PortDeclaration
--     } | MINonPortModuleItem {
--         nonPortModuleItem :: NonPortModuleItem
--     }

-- data PortDeclaration =
--     PDInoutDeclaration {
--         attributeInstances :: [AttributeInstance],
--         inoutDeclaration :: InoutDeclaration
--     } | PDInputDeclaration {
--         attributeInstances :: [AttributeInstance],
--         inputDeclaration :: InputDeclaration
--     } | PDOutputDeclaration {
--         attributeInstances :: [AttributeInstance],
--         outputDeclaration :: OutputDeclaration
--     } | PDRefDeclaration {
--         attributeInstances :: [AttributeInstance],
--         refDeclaration :: RefDeclaration
--     } | PDInterfacePortDeclaration {
--         attributeInstances :: [AttributeInstance],
--         interfacePortDeclaration :: InterfacePortDeclaration
--     }

-- data InoutDeclaration =
--     InoutDeclaration {
--         inoutnetPortType :: InoutnetPortType,
--         portIdentifiers :: [PortIdentifier]
--     }

-- data PortIdentifier =
--     PortIdentifier {
--         name :: Identifier,
--         unpackedDimension :: [UnpackedDimension]
--     }

-- data UnpackedDimension =
--     UDConstantRange {
--         constantRange :: ConstantRange
--     } | UDConstantExpression {
--         constantExpression :: ConstantExpression
--     }

-- data ConstantRange =
--     CR{
--         beg :: ConstantExpression,
--         end :: ConstantExpression
--     }

-- data ConstantExpression =
--     ConstantExpression{
--         constantPrimary :: ConstantPrimary
--     }


-- -- | Incomplete production rule
-- data NonPortModuleItem =
--     NPMIGenerateRegion {
--         generateRegion :: GenerateRegion
--     } | NPMIModuleOrGenerateItem {
--         moduleOrGenerateItem :: ModuleOrGenerateItem
--     }

-- -- | Incomplete production rule
-- data ModuleOrGenerateItem =
--     MOGIModuleCommonItem{
--         attributeInstances :: [AttributeInstance],
--         moduleCommonItem :: ModuleCommonItem
--     }

-- -- | Incomplete production rule
-- data ModuleCommonItem =
--     MCIModuleOrGenerateItemDeclaration {
--         moduleOrGenerateItem :: ModuleOrGenerateItem
--     } | MCIAlwaysConstruct {
--         alwaysConstruct :: AlwaysConstruct
--     }

-- data AlwaysConstruct =
--     AlwaysConstruct {alwaysKeyword :: AlwaysKeyword, statement :: Statement}

-- data AlwaysKeyword = Always | AlwaysComb | AlwaysLatch | AlwaysFF

-- data Statement =
--     Statement {
--         blockIdentifier :: Maybe BlockIdentifier,
--         attributeInstances :: [AttributeInstance],
--         statementItem :: StatementItem
--     }

-- -- | Incomplete production rule
-- data StatementItem =
--     SIBlockingAssignment {
--         blockingAssignment :: BlockingAssignment
--     } | SINonblockingAssignment {
--         nonblockingAssignment :: NonBlockingAssignment
--     }

-- -- | Incomplete production rule
-- data BlockingAssignment =
--     BAOperatorAssignment {
--         operatorAssignment :: OperatorAssignment
--     }

-- data OperatorAssignment  =
--     OperatorAssignment {
--         variableLvalue :: VariableLvalue,
--         assignmentOperator :: AssignmentOperator,
--         expression :: Expression
--     }

-- data Expression =
--     EPrimary {
--         primary :: Primary
--     } | EUnaryOperator {
--         unaryOperator :: UnaryOperator,
--         attributeInstances :: [AttributeInstance],
--         primary :: Primary
--     } | EIncDec {
--         incOrDecExpression :: IncOrDecExpression
--     } | EOperatorAssignment {
--         operatorAssignment :: OperatorAssignment
--     } | EBinaryOperator {
--         subExpressions :: (Expression,Expression),
--         binaryOperator :: BinaryOperator,
--         attributeInstances :: [AttributeInstance] ,
--         conditionalExpression :: ConditionalExpression
--     } | EInsideExpression {
--         insideExpression :: InsideExpression
--     } | ETaggedUnionExpression {
--         taggedUnionExpression :: TaggedUnionExpression
--     }

-- data Primary =
--     PPrimaryLiteral { primaryLiteral :: PrimaryLiteral }
--     | PPrimaryHierarchicalIdentifier {
--         scopeQualifier :: Maybe ScopeQualifier,
--         hierarchicalIdentifier :: HierarchicalIdentifier,
--         select :: Select
--     } | PEmptyQueue
--     | PConcatenation {
--         rangeExpression :: Maybe RangeExpression
--     } | PMultipleConcatenation {
--         rangeExpression :: Maybe RangeExpression
--     } | PFunctionSubroutineCall {
--         functionSubRoutineCall :: FunctionSubroutineCall
--     } | PLetExpression {
--         letExpression :: LetExpression
--     } | PMintypmaxExpression {
--         mintypemaxExpression :: MinTypMaxExpression
--     } | PCast {
--         cast :: Cast
--     } | PAssignmentPatternExpression {
--         assignmentPatternExpression :: AssignmentPatternExpression
--     } | PStreamingConcatenation | PSequenceMethodCall | PThis | PDollar | PNull

-- data MinTypMaxExpression =
--     MTMESingleExpression {
--         expression :: Expression
--     } | MTMETrippleExpression {
--         expressionA :: Expression,
--         expressionB :: Expression,
--         expressionC :: Expression
--     }

-- data ScopeQualifier =
--     SQClassQualifier {
--         classQualifier :: ClassQualifier
--     }
--     | SQPackageScope { packageScope :: PackageScope }

-- data AssignmentOperator =
--     Equals
--     | PlusEquals
--     | MinusEquals
--     | ProductEquals
--     | DivideEquals
--     | PercentageEquals
--     | AmpersandEquals
--     | PipeEquals
--     | CaretEquals
--     | ShiftLeftEqals
--     | ShiftRightEquals
--     | ArithmeticShiftLeftEquals
--     | ArithmeticShiftRightEquals


-- data PrimaryLiteral = PrimaryLiteral
-- data ModuleAnsiHeader = ModuleAnsiHeader
-- data AttributeInstance = AttributeInstance
-- data Lifetime = Lifetime
-- data ModuleIdentifier = ModuleIdentifier
-- data GenerateRegion = GenerateRegion
-- data ClassQualifier = ClassQualifier
-- data UnaryOperator = UnaryOperator
-- data PackageScope = PackageScope
-- data Cast = Cast
-- data AssignmentPatternExpression = AssignmentPatternExpression
-- data IncOrDecExpression = IncOrDecExpression
-- data UdpDeclaration = UdpDeclaration
-- data InterfaceDeclaration = InterfaceDeclaration
-- data ProgramDeclaration = ProgramDeclaration
-- data PackageDeclaration = PackageDeclaration
-- data PackageItem = PackageItem
-- data BindDirective = BindDirective
-- data ConfigDeclaration = ConfigDeclaration
-- data InputDeclaration = InputDeclaration
-- data OutputDeclaration = OutputDeclaration
-- data RefDeclaration = RefDeclaration
-- data InterfacePortDeclaration = InterfacePortDeclaration
-- data InoutnetPortType = InoutnetPortType
-- data Identifier = Identifier
-- data ConstantPrimary = ConstantPrimary
-- data BlockIdentifier = BlockIdentifier
-- data NonBlockingAssignment = NonBlockingAssignment
-- data VariableLvalue = VariableLvalue
-- data BinaryOperator = BinaryOperator
-- data ConditionalExpression = ConditionalExpression
-- data InsideExpression = InsideExpression
-- data TaggedUnionExpression = TaggedUnionExpression
-- data HierarchicalIdentifier = HierarchicalIdentifier
-- data Select = Select
-- data RangeExpression = RangeExpression
-- data FunctionSubroutineCall = FunctionSubroutineCall
-- data LetExpression = LetExpression