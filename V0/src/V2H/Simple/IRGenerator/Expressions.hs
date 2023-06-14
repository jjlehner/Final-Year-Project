module V2H.Simple.IRGenerator.Expressions where
import Control.Lens
import Data.Map qualified as Map
import Data.Bits qualified as Bits
import V2H.Simple.Ast qualified as SimpleAst
import V2H.IR qualified as IR
import V2H.IR.DataTypes qualified as IR
import Text.Pretty.Simple
import Data.Text.Lazy (unpack)
import Debug.Trace

fromBoolToInteger :: Bool -> Integer
fromBoolToInteger False = 0
fromBoolToInteger True = 1

mkSignalValueDataObjectFromInteger :: Integer -> IR.SVDataObject
mkSignalValueDataObjectFromInteger i =
    IR.SVDataObject {
        IR.unaryOpMinus = \res -> mkSignalValueDataObjectFromInteger $ (-i) `mod` 2 ^ IR.getBitWidth res,
        IR.unaryOpExclamationMark = \_ -> mkSignalValueDataObjectFromInteger $ fromBoolToInteger $ i == 0 ,
        IR.binaryOpPlus =
            \res b -> mkSignalValueDataObjectFromInteger $ (i + IR.objToInteger b res) `mod` 2 ^ IR.getBitWidth res,
        IR.getLSB = Bits.testBit i 0,
        IR.objToInteger = \res -> i `mod` 2 ^ IR.getBitWidth res,
        IR.objToString = show i,
        IR.binaryEqualEqual = \dataType other -> i == IR.objToInteger other dataType
    }

generateExpression ::
    IR.VariableMapIR
    -> IR.NetMapIR
    -> SimpleAst.Expression
    -> IR.ExpressionIR
generateExpression _ _ (SimpleAst.ELiteral i) =
    IR.ELiteral $ IR.SignalValue (IR.DTSingular $ IR.STInteger IR.NSITInt) $ mkSignalValueDataObjectFromInteger i
generateExpression variables nets (SimpleAst.EConnection (SimpleAst.VariableIdentifier identifier) Nothing Nothing) =
    let variable = Map.lookup (IR.VariableOrNetIdentifierIR identifier) variables
        net = Map.lookup (IR.VariableOrNetIdentifierIR identifier) nets
    in case (variable, net) of
        (Nothing, Nothing) -> undefined
        (Just v, Just n) -> undefined
        (Just v, Nothing) -> IR.EConnection $ IR.ConnectionVariableIR (IR.I v.identifier) Nothing
        (Nothing, Just n) -> IR.EConnection $ IR.ConnectionNetIR (IR.I n.identifier) Nothing

generateExpression variables nets (SimpleAst.EUnaryOperator b exp) =
    IR.EUnaryOperator (generateUnaryOperator b) $ generateExpression variables nets exp
generateExpression variables nets (SimpleAst.EBinaryOperator b exp1 exp2) =
    IR.EBinaryOperator (generateBinaryOperator b) (generateExpression variables nets exp1) (generateExpression variables nets exp2)
generateUnaryOperator x =
    case x of
        SimpleAst.UOExclamationMark -> IR.UOLogicalNot
        SimpleAst.UOTilde-> IR.UOBitwiseNot

generateBinaryOperator x =
    case x of
        SimpleAst.BOPlus -> IR.BOPlus
        SimpleAst.BOMinus -> IR.BOMinus