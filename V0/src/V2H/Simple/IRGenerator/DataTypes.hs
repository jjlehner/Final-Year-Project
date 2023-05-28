module V2H.Simple.IRGenerator.DataTypes where
import V2H.Simple.Ast as SimpleAst
import V2H.IR qualified as IR
import V2H.IR.DataTypes qualified as IR

generateAstIntegerVectorType :: SimpleAst.IntegerVectorType -> IR.DataTypeIR
generateAstIntegerVectorType IVTBit = IR.DTSingular $ IR.STScalar IR.SIVTBit
generateAstIntegerVectorType IVTLogic = IR.DTSingular $ IR.STScalar IR.SIVTLogic
generateAstIntegerVectorType IVTReg = IR.DTSingular $ IR.STScalar IR.SIVTReg

generateDataType :: SimpleAst.DataType -> IR.DataTypeIR
generateDataType astDataType =
    case astDataType of
        DTIntegerVector ivt -> generateAstIntegerVectorType ivt