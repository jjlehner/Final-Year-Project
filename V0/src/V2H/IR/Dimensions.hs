module V2H.IR.Dimensions where
import GHC.Generics

data PackedDimensionIR = PackedDimensionIR deriving (Show, Eq, Ord, Generic)
data UnpackedDimensionIR = UnpackedDimensionIR deriving (Show, Eq, Ord, Generic)