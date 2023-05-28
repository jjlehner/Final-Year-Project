module V2H.Simple.IRGenerator where

import Control.Lens
import Data.Set qualified as Set
import Data.Map qualified as Map
import Data.List qualified as List
import Data.Generics.Product
import Data.Maybe qualified as Maybe
import Debug.Trace
import V2H.Simple.Ast qualified as SimpleAst
import V2H.IR qualified as IR
import V2H.Simple.IRGenerator.DataTypes
import V2H.Simple.IRGenerator.Expressions ( generateExpression )

generateVariableOrNetIdentifier ::
    SimpleAst.VariableIdentifier
    -> IR.VariableOrNetIdentifierIR
generateVariableOrNetIdentifier (SimpleAst.VariableIdentifier iden) = IR.VariableOrNetIdentifierIR iden

generateConnection ::
    IR.VariableMapIR
    -> SimpleAst.VariableLvalue
    -> IR.ConnectionIR
generateConnection variables (SimpleAst.VariableLvalue v Nothing partSelectRange) =
    IR.ConnectionVariableIR (variables Map.! generateVariableOrNetIdentifier v) Nothing

generateStatementItemIRs ::
    IR.VariableMapIR
    -> IR.NetMapIR
    -> SimpleAst.StatementItem
    -> [IR.StatementItemIR]
generateStatementItemIRs variables nets (SimpleAst.SIBlockingAssignment blockingAssignment) =
    let conn = generateConnection variables blockingAssignment.variableLvalue
        expr = generateExpression variables nets blockingAssignment.expression
    in List.singleton $ IR.BlockingAssignment conn expr

generateStatementItemIRs variables nets (SimpleAst.SISeqBlock (SimpleAst.SeqBlock statements)) =
    concatMap (generateStatementItemIRs variables nets) statements


generateAlwaysConstructIR ::
    IR.VariableMapIR
    -> IR.NetMapIR
    -> SimpleAst.ModuleIdentifier
    -> Int
    -> SimpleAst.AlwaysConstruct
    -> (IR.AlwaysConstructIdentifierIR, IR.AlwaysConstructIR)
generateAlwaysConstructIR variables nets (SimpleAst.ModuleIdentifier heldInModule) num (SimpleAst.ACComb statementItem) =
    let iden = IR.AlwaysConstructIdentifierIR $ heldInModule ++ ".always_construct." ++ show num
        statementItemIRs = generateStatementItemIRs variables nets statementItem
    in (iden, IR.AlwaysConstructIR {
        identifier = iden,
        sensitivity = IR.Comb,
        inputConnections =  Set.fromList $ concatMap IR.getInputToStatementIR statementItemIRs,
        outputConnections = Set.fromList $ concatMap IR.getOutputToStatementIR statementItemIRs,
        statementItems = statementItemIRs
    })
mapi f m = uncurry f <$> zip [0..] m

generateAlwaysConstructIRs ::
    IR.VariableMapIR
    -> IR.NetMapIR
    -> SimpleAst.ModuleDeclaration
    -> IR.AlwaysConstructMapIR
generateAlwaysConstructIRs variables nets moduleDeclaration =
    toListOf (types @SimpleAst.AlwaysConstruct) moduleDeclaration
    & mapi (generateAlwaysConstructIR variables nets moduleDeclaration.moduleHeader.moduleIdentifier)
    & Map.fromList

generateInnerVariableIRs ::
    [SimpleAst.DataDeclaration]
    -> IR.VariableMapIR
generateInnerVariableIRs dataDeclarations =
    let variableIdentifier (SimpleAst.VariableIdentifier iden) = IR.VariableOrNetIdentifierIR iden
        generateVariable dataDeclaration =
            (variableIdentifier dataDeclaration.variableIdentifier,
            IR.VariableIR {
                identifier = variableIdentifier dataDeclaration.variableIdentifier,
                dataType = generateDataType dataDeclaration.dataType
            })
    in Map.fromList $ fmap generateVariable dataDeclarations

generatePortDirection SimpleAst.PDInput = IR.PDInput
generatePortDirection SimpleAst.PDOutput = IR.PDOutput

generatePortIdentifier (SimpleAst.PortIdentifier iden) = IR.PortIdentifierIR iden

generatePortVariableIR ::
    SimpleAst.PortDeclaration
    -> Maybe (IR.VariableOrNetIdentifierIR, IR.VariableIR)
generatePortVariableIR SimpleAst.PortDeclaration{netType=Just netType} = Nothing
generatePortVariableIR SimpleAst.PortDeclaration{dataType=Nothing} = Nothing
generatePortVariableIR
    SimpleAst.PortDeclaration{
        dataType=(Just dataType),
        portIdentifier=(SimpleAst.PortIdentifier portIdentifier)
    } =
        Just (IR.VariableOrNetIdentifierIR portIdentifier, IR.VariableIR {
            identifier = IR.VariableOrNetIdentifierIR portIdentifier,
            dataType = generateDataType dataType
        })

generatePortVariableIRs ::
    [SimpleAst.PortDeclaration]
    -> IR.VariableMapIR
generatePortVariableIRs portDeclarations =
    Map.fromList $ Maybe.mapMaybe generatePortVariableIR portDeclarations

generateVariableIRs moduleDeclaration =
    let nestedVariables = generateInnerVariableIRs $ toListOf (types @SimpleAst.DataDeclaration) moduleDeclaration
        portVariables = generatePortVariableIRs $ toListOf (types @SimpleAst.PortDeclaration) moduleDeclaration
    in  Map.union nestedVariables portVariables

generateNetIRs moduleDeclaration = Map.empty

generatePortDeclarationIR ::
    IR.VariableMapIR
    -> IR.NetMapIR
    -> SimpleAst.PortDeclaration
    -> (IR.PortIdentifierIR, IR.PortDeclarationIR)
generatePortDeclarationIR variables nets portDeclaration =
    let (SimpleAst.PortIdentifier iden) = portDeclaration.portIdentifier
        variableNetIden = IR.VariableOrNetIdentifierIR iden
        portIden = generatePortIdentifier portDeclaration.portIdentifier
    in (portIden,IR.PortDeclarationIR{
        portIdentifier = portIden,
        portDirection = generatePortDirection portDeclaration.portDirection,
        connection = if Maybe.isJust portDeclaration.netType || Maybe.isNothing portDeclaration.dataType then
                        IR.ConnectionNetIR ((Map.!) nets variableNetIden) Nothing
                     else IR.ConnectionVariableIR ((Map.!) variables variableNetIden) Nothing
    })

generatePortDeclarationIRs ::
    IR.VariableMapIR
    -> IR.NetMapIR
    -> SimpleAst.ModuleDeclaration
    -> IR.PortDeclarationMapIR
generatePortDeclarationIRs variables nets moduleDeclaration =
    let portDeclarationList = Maybe.fromMaybe [] $ moduleDeclaration.moduleHeader.portDeclarations
    in Map.fromList $ fmap (generatePortDeclarationIR variables nets) portDeclarationList

a :: SimpleAst.PortConnection -> (IR.PortIdentifierIR, IR.ExpressionIR)
a portConnection =


generateSubmoduleIR ::
    SimpleAst.ModuleInstantiation
    -> IR.SubmoduleIR
generateSubmoduleIR moduleInstantiation =
    IR.SubmoduleIR {
        submoduleIdentifier = generateModuleIdentifier moduleInstantiation.moduleIdentifier,
        connections = moduleInstantiation.hierarchicalInstance.
    }

generateSubmoduleIRs ::
    SimpleAst.ModuleDeclaration
    -> [IR.SubmoduleIR]
generateSubmoduleIRs moduleDeclaration =
    fmap generateSubmoduleIR $ toListOf (types @SimpleAst.ModuleInstantiation) sourceText

generateModuleIR ::
    SimpleAst.ModuleDeclaration
    -> IR.IR
generateModuleIR moduleDeclaration =
    let variables = generateVariableIRs moduleDeclaration
        nets = generateNetIRs moduleDeclaration
        alwaysConstructs = generateAlwaysConstructIRs variables nets moduleDeclaration
        portsTable = generatePortDeclarationIRs variables nets moduleDeclaration
        generateModuleIdentifier (SimpleAst.ModuleIdentifier iden) = IR.ModuleIdentifierIR iden
    in traceShow variables IR.IR {
        moduleIdentifier = generateModuleIdentifier moduleDeclaration.moduleHeader.moduleIdentifier,
        alwaysConstructs = alwaysConstructs,
        variables = variables,
        nets = Map.empty,
        ports = portsTable
    }

generateIR ::
    SimpleAst.SourceText
    -> Either String [IR.IR]
generateIR sourceText =
    Right $ generateModuleIR <$> toListOf (types @SimpleAst.ModuleDeclaration) sourceText