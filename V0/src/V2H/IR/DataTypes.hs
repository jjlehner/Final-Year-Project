module V2H.IR.DataTypes where

import GHC.Generics
import Language.Haskell.TH.Syntax

import V2H.IR.Dimensions
import Debug.Trace

data StructIR = StructIR deriving (Show, Eq, Ord, Generic, Lift )
data UnionIR = UnionIR deriving (Show, Eq, Ord, Generic, Lift)

data DataTypeIR = DTSingular SingularTypeIR
                  | DTAggregate AggregateTypeIR
                  | DTUnit deriving (Show, Eq, Ord, Generic, Lift)

data PackedArraySubTypeIR =
    PATScalar ScalarIntegerVectorTypeIR
    | PATNestedPAT PackedDimensionIR PackedArraySubTypeIR
    | PATStruct StructIR
    | PATUnion UnionIR deriving (Show, Eq, Ord, Generic, Lift)

data ScalarIntegerVectorTypeIR = SIVTBit | SIVTLogic | SIVTReg deriving (Show, Eq, Ord, Generic, Lift)
data NonScalarIntegerTypeIR = NSCITShortInt | NSITInt | NSCITLongint | NSCITByte | NSCITInteger | NSCITTime deriving (Show, Eq, Ord, Generic, Lift)
data SingularTypeIR =
    STPackedStruct StructIR
    | STPackedUnion UnionIR
    | STScalar ScalarIntegerVectorTypeIR
    | STPackedArray PackedDimensionIR PackedArraySubTypeIR
    | STInteger NonScalarIntegerTypeIR deriving (Show, Eq, Ord, Generic, Lift)

data AggregateTypeIR =
    ATUnpackedStructure UnpackedDimensionIR StructIR
    | ATUnpackedUnion UnpackedDimensionIR UnionIR
    | ATUnpackedArray UnpackedDimensionIR DataTypeIR deriving (Show, Eq, Ord, Generic, Lift)

getBitWidthOfPackedArraySubTypeIR :: PackedArraySubTypeIR -> Integer
getBitWidthOfPackedArraySubTypeIR (PATNestedPAT p n) = getBitWidthOfPackedDimension p * getBitWidthOfPackedArraySubTypeIR n
getBitWidthOfPackedArraySubTypeIR (PATScalar _) = 1

getBitWidthOfPackedDimension (PackedDimensionIR a b) = a - b + 1

getBitWidthOfSingularTypeIR :: SingularTypeIR -> Integer
getBitWidthOfSingularTypeIR (STScalar _)  = 1
getBitWidthOfSingularTypeIR (STPackedArray p s) = getBitWidthOfPackedDimension p * getBitWidthOfPackedArraySubTypeIR s
getBitWidthOfSingularTypeIR (STInteger NSITInt) = 32

getBitWidth :: DataTypeIR -> Integer
getBitWidth (DTSingular s) = getBitWidthOfSingularTypeIR s
getBitWidth DTUnit = 0

concatDataType :: DataTypeIR -> DataTypeIR -> DataTypeIR
concatDataType (DTSingular (STPackedArray (PackedDimensionIR a 0) (PATScalar SIVTLogic))) (DTSingular (STPackedArray (PackedDimensionIR b 0) (PATScalar SIVTLogic))) =
    DTSingular $ STPackedArray (PackedDimensionIR (a+b) 0) (PATScalar SIVTLogic)
concatDataType DTUnit a = a
concatDataType a DTUnit = a
concatDataType (DTSingular (STPackedArray (PackedDimensionIR a 0) (PATScalar SIVTLogic))) (DTSingular (STScalar SIVTLogic)) =
    DTSingular $ STPackedArray (PackedDimensionIR (a+1) 0) (PATScalar SIVTLogic)
concatDataType (DTSingular (STScalar SIVTLogic)) (DTSingular (STPackedArray (PackedDimensionIR a 0) (PATScalar SIVTLogic))) =
    DTSingular $ STPackedArray (PackedDimensionIR (a+1) 0) (PATScalar SIVTLogic)
concatDataType (DTSingular (STScalar SIVTLogic)) (DTSingular (STScalar SIVTLogic)) = DTSingular $ STPackedArray (PackedDimensionIR 1 0) (PATScalar SIVTLogic)