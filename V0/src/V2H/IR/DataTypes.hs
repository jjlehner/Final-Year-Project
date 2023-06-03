module V2H.IR.DataTypes where

import GHC.Generics
import V2H.IR.Dimensions

data StructIR = StructIR deriving (Show, Eq, Ord, Generic)
data UnionIR = UnionIR deriving (Show, Eq, Ord, Generic)

data DataTypeIR = DTSingular SingularTypeIR
                  | DTAggregate AggregateTypeIR deriving (Show, Eq, Ord, Generic)

data PackedArrayTypeIR =
    PATScalar ScalarIntegerVectorTypeIR
    | PATNestedPAT PackedDimensionIR PackedArrayTypeIR
    | PATStruct StructIR
    | PATUnion UnionIR deriving (Show, Eq, Ord, Generic)

data ScalarIntegerVectorTypeIR = SIVTBit | SIVTLogic | SIVTReg deriving (Show, Eq, Ord, Generic)
data NonScalarIntegerTypeIR = NSCITShortInt | NSITInt | NSCITLongint | NSCITByte | NSCITInteger | NSCITTime deriving (Show, Eq, Ord, Generic)
data SingularTypeIR =
    STPackedStruct StructIR
    | STPackedUnion UnionIR
    | STScalar ScalarIntegerVectorTypeIR
    | STPackedArray PackedDimensionIR PackedArrayTypeIR
    | STInteger NonScalarIntegerTypeIR deriving (Show, Eq, Ord, Generic)

data AggregateTypeIR =
    ATUnpackedStructure UnpackedDimensionIR StructIR
    | ATUnpackedUnion UnpackedDimensionIR UnionIR
    | ATUnpackedArray UnpackedDimensionIR DataTypeIR deriving (Show, Eq, Ord, Generic)

getBitWidth :: DataTypeIR -> Integer
getBitWidth (DTSingular (STScalar SIVTLogic)) = 1
