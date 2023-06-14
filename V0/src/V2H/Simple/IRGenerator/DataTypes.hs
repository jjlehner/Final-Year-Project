module V2H.Simple.IRGenerator.DataTypes where
import V2H.Simple.Ast as SimpleAst
import V2H.IR qualified as IR
import V2H.IR.DataTypes qualified as IR
import V2H.IR.Dimensions qualified as IR

generateScalarIntegerVectorType :: SimpleAst.IntegerVectorType -> IR.ScalarIntegerVectorTypeIR
generateScalarIntegerVectorType IVTBit = IR.SIVTBit
generateScalarIntegerVectorType IVTLogic = IR.SIVTLogic
generateScalarIntegerVectorType IVTReg = IR.SIVTReg

generatePackedDimension :: SimpleAst.PackedDimension -> IR.PackedDimensionIR
generatePackedDimension (SimpleAst.PackedDimension (SimpleAst.ConstantRange start end)) = IR.PackedDimensionIR start end

generatePackedArraySubType :: SimpleAst.DataType -> IR.PackedArraySubTypeIR
generatePackedArraySubType astDataType =
    case astDataType of
        (SimpleAst.DTIntegerVector ivt []) -> IR.PATScalar $ generateScalarIntegerVectorType ivt
        (SimpleAst.DTIntegerVector ivt (h:t))
            -> IR.PATNestedPAT (generatePackedDimension h) $ generatePackedArraySubType $ SimpleAst.DTIntegerVector ivt t

generateDataType :: SimpleAst.DataType -> IR.DataTypeIR
generateDataType astDataType =
    case astDataType of
        DTIntegerVector ivt [] -> IR.DTSingular $ IR.STScalar $ generateScalarIntegerVectorType ivt
        DTIntegerVector ivt (h:t)
            -> IR.DTSingular $ IR.STPackedArray (generatePackedDimension h) $ generatePackedArraySubType $ DTIntegerVector ivt t