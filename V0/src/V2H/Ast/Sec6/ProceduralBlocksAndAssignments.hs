module V2H.Ast.Sec6.ProceduralBlocksAndAssignments where

import V2H.Ast.Sec6.Statements
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