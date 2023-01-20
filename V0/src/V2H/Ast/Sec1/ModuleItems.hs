module V2H.Ast.Sec1.ModuleItems where
import V2H.Ast.Sec1.ModuleParametersAndPorts
import V2H.Ast.Sec4.InterfaceInstantiation
import V2H.Ast.Sec4.ProgramInstantiation
import V2H.Ast.Sec4.GeneratedInstantiation
import V2H.Ast.Sec6.AssertionStatements
import V2H.Ast.Sec6.ContinuousAssignmentAndNetAliasStatements
import V2H.Ast.Sec6.ProceduralBlocksAndAssignments

-- | Incomplete production rule
data BindDirective = BindDirective

data ModuleItem =
    MIPort_Declaration {
        portDeclaration :: PortDeclaration
    } | MINonPortModuleItem {
        nonPortModuleItem :: NonPortModuleItem
    }

data NonPortModuleItem =
    NPMIModuleOrGenerateItemDeclaration {
       moduleOrGenerateItemDeclaration :: ModuleOrGenerateItemDeclaration
    } | NPMIInterfaceInstantiation {
        interfaceInstantiation :: InterfaceInstantiation
    } | NPMIProgramInstantiation {
        programInstantiation :: ProgramInstantiation
    } | NPMIAssertionItem {
        assertionItem :: AssertionItem
    } | NPMIBindDirective {
        bindDirective :: BindDirective
    } | NPMIContinuousAssign {
        continuousAssign :: ContinuousAssign
    } | NPMINetAlias {
        netAlias :: NetAlias
    } | NPMIInitialConstruct {
        initialConstruct :: InitialConstruct
    } | NPMIFinalConstruct {
        finalConstruct :: FinalConstruct
    } | NPMIAlwaysConstruct {
        alwaysConstruct :: AlwaysConstruct
    } | NPMILoopGenerateConstruct {
        loopGenerateConstruct :: LoopGenerateConstruct
    } | NPMIConditionalGenerateConstruct {
        conditionalGenerateConstruct :: ConditionalGenerateConstruct
    } | NPMIElaborationSystemTask {
        elaborationSystemTask :: ElaborationSystemTask
    }
-- | Incomplete production rule
data ModuleOrGenerateItemDeclaration = ModuleOrGenerateItemDeclaration

-- | Incomplete production rule
data ElaborationSystemTask = ElaborationSystemTask