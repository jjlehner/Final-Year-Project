module V2H.IR.Dimensions where
import GHC.Generics

import Language.Haskell.TH.Syntax

data PackedDimensionIR = PackedDimensionIR deriving (Show, Eq, Ord, Generic, Lift)
data UnpackedDimensionIR = UnpackedDimensionIR deriving (Show, Eq, Ord, Generic, Lift)