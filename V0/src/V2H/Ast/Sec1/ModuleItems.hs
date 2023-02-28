module V2H.Ast.Sec1.ModuleItems where
import V2H.Ast.Sec1.ModuleParametersAndPorts
import V2H.Ast.Sec4.InterfaceInstantiation
import V2H.Ast.Sec4.ProgramInstantiation
import V2H.Ast.Sec4.GeneratedInstantiation
import V2H.Ast.Sec6.AssertionStatements
import V2H.Ast.Sec6.ContinuousAssignmentAndNetAliasStatements
import V2H.Ast.Sec6.ProceduralBlocksAndAssignments

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
data ModuleOrGenerateItem = MORGIModuleCommonItem{
        attributeInstances :: [AttributeInstance],
        moduleCommonItem :: ModuleCommonItem
    }

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
        moduleDeclaration :: ModuleDeclartion
    } | NPMIInterfaceDeclaration {
        interfaceDeclaration :: InterfaceDeclaration
    } | NPMITimeunitsDeclaration {
        timeunitsDeclaration :: TimeunitsDeclaration
    }

-- | Incomplete production rule
data ModuleOrGenerateItemDeclaration = ModuleOrGenerateItemDeclaration deriving (Show)

-- | Incomplete production rule
data ElaborationSystemTask = ElaborationSystemTask deriving (Show)