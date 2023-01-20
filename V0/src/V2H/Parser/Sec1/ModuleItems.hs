module V2H.Parser.Sec1.ModuleItems where
import V2H.Parser.Sec1.ModuleParametersAndPorts
import V2H.Parser.Sec4.InterfaceInstantiation
import V2H.Parser.Sec4.ProgramInstantiation
import V2H.Parser.Sec4.GeneratedInstantiation
import V2H.Parser.Sec6.AssertionStatements
import V2H.Parser.Sec6.ContinuousAssignmentAndNetAliasStatements
import V2H.Parser.Sec6.ProceduralBlocksAndAssignments

import V2H.Ast.Sec1.ModuleItems
import Text.Parsec

-- | Incomplete production rule
bindDirectiveNT = undefined

moduleItemNT = undefined

-- | Incomplete production rule
nonPortModuleItemNT :: Parsec String u NonPortModuleItem
nonPortModuleItemNT = NPMIAlwaysConstruct <$> alwaysConstructNT

-- | Incomplete production rule
moduleOrGenerateItemDeclarationNT = undefined

-- | Incomplete production rule
elaborationSystemTaskNT = undefined