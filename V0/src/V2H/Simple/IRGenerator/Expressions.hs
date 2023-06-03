module V2H.Simple.IRGenerator.Expressions where
import Control.Lens
import Data.Map qualified as Map
import Data.Bits qualified as Bits
import V2H.Simple.Ast qualified as SimpleAst
import V2H.IR qualified as IR
import V2H.IR.DataTypes qualified as IR

fromBoolToInteger :: Bool -> Integer
fromBoolToInteger False = 0
fromBoolToInteger True = 1

mkSignalValueFromInteger :: Integer -> IR.SVDataObject
mkSignalValueFromInteger i =
    IR.SVDataObject {
        unaryOpMinus = mkSignalValueFromInteger . objIntegerToInteger (-i) ,
        unaryOpExclamationMark = \_ -> mkSignalValueFromInteger $ fromBoolToInteger $ i /= 0 ,
        getLSB = Bits.testBit i 0,
        objToInteger = objIntegerToInteger i,
        objToString = show i,
        binaryEqualEqual = \dataType other -> i == IR.objToInteger other dataType
    }

objIntegerToInteger :: Integer -> IR.DataTypeIR -> Integer
objIntegerToInteger i (IR.DTSingular (IR.STScalar sivt)) = i
objIntegerToInteger _ _= undefined

generateExpression ::
    IR.VariableMapIR
    -> IR.NetMapIR
    -> SimpleAst.Expression
    -> IR.ExpressionIR
generateExpression _ _ (SimpleAst.ELiteral i) =
    IR.ELiteral $ IR.SignalValue (IR.DTSingular $ IR.STInteger IR.NSITInt) $ mkSignalValueFromInteger i
generateExpression variables nets (SimpleAst.EConnection (SimpleAst.VariableIdentifier identifier) Nothing Nothing) =
    let variable = Map.lookup (IR.VariableOrNetIdentifierIR identifier) variables
        net = Map.lookup (IR.VariableOrNetIdentifierIR identifier) nets
    in case (variable, net) of
        (Nothing, Nothing) -> undefined
        (Just v, Just n) -> undefined
        (Just v, Nothing) -> IR.EConnection $ IR.ConnectionVariableIR v Nothing
        (Nothing, Just n) -> IR.EConnection $ IR.ConnectionNetIR n Nothing