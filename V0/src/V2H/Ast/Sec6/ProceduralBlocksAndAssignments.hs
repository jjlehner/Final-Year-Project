module V2H.Ast.Sec6.ProceduralBlocksAndAssignments where

import V2H.Ast.Sec6.Statements
-- | Incomplete production rule
data InitialConstruct = InitialConstruct

-- | Incomplete production rule
data AlwaysConstruct =
    AlwaysConstruct {
        alwaysKeyword :: AlwaysKeyword,
        statement :: Statement
    }

-- | Incomplete production rule
data FinalConstruct = FinalConstruct

data AlwaysKeyword = Always | AlwaysComb | AlwaysLatch | AlwaysFF

-- | Incomplete production rule
data BlockingAssignment = BlockingAssignment

-- | Incomplete production rule
data OperatorAssignment = OperatorAssignment

-- | Incomplete production rule
data AssignmentOperator = AssignmentOperator

-- | Incomplete production rule
data NonblockingAssignment = NonblockingAssignment

-- | Incomplete production rule
data ProceduralContinuousAssignment = ProceduralContinuousAssignment

-- | Incomplete production rule
data VariableAssignment = VariableAssignment