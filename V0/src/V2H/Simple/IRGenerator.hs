{-# LANGUAGE DuplicateRecordFields#-}
module V2H.Simple.IRGenerator where

import Control.Lens
import              Data.Generics.Product
import Data.Set qualified as Set
import Data.Map qualified as Map
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Debug.Trace
import V2H.Simple.Ast qualified as SimpleAst
import V2H.IR qualified as IR
import V2H.Simple.IRGenerator.DataTypes
import V2H.Simple.IRGenerator.Expressions
import V2H.Simple.IRGenerator.Identifiers

generateConnection ::
    SimpleAst.VariableLvalue
    -> IR.ConnectionIR
generateConnection (SimpleAst.VariableLvalue v Nothing partSelectRange) =
    IR.ConnectionVariableIR (IR.I $ generateVariableOrNetIdentifier v) Nothing

generateStatementItemIRs ::
    IR.VariableMapIR
    -> IR.NetMapIR
    -> SimpleAst.StatementItem
    -> [IR.StatementItemIR]
generateStatementItemIRs variables nets (SimpleAst.SIBlockingAssignment blockingAssignment) =
    let conn = generateConnection blockingAssignment.variableLvalue
        expr = generateExpression variables nets blockingAssignment.expression
    in List.singleton $ IR.BlockingAssignment conn expr

generateStatementItemIRs variables nets (SimpleAst.SISeqBlock (SimpleAst.SeqBlock statements)) =
    concatMap (generateStatementItemIRs variables nets) statements


generateAlwaysConstructIR ::
    IR.VariableMapIR
    -> IR.NetMapIR
    -> Int
    -> SimpleAst.AlwaysConstruct
    -> (IR.AlwaysConstructIdentifierIR, IR.AlwaysConstructIR)
generateAlwaysConstructIR variables nets num (SimpleAst.ACComb statementItem) =
    let iden = IR.AlwaysConstructIdentifierIR $  "always_construct." ++ show num
        statementItemIRs = generateStatementItemIRs variables nets statementItem
    in (iden, IR.AlwaysConstructIR {
        _alwaysConstructIdentifier = iden,
        _sensitivity = IR.Comb,
        _inputConnections =  Set.fromList $ concatMap IR.getInputToStatementIR statementItemIRs,
        _outputConnections = Set.fromList $ concatMap IR.getOutputToStatementIR statementItemIRs,
        _statementItems = statementItemIRs
    })
mapi f m = uncurry f <$> zip [0..] m

generateAlwaysConstructIRs ::
    IR.VariableMapIR
    -> IR.NetMapIR
    -> SimpleAst.ModuleDeclaration
    -> IR.AlwaysConstructMapIR
generateAlwaysConstructIRs variables nets moduleDeclaration =
    toListOf (types @SimpleAst.AlwaysConstruct) moduleDeclaration
    & mapi (generateAlwaysConstructIR variables nets)
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
                        IR.ConnectionNetIR (IR.I variableNetIden) Nothing
                     else IR.ConnectionVariableIR (IR.I variableNetIden) Nothing
    })

generatePortDeclarationIRs ::
    IR.VariableMapIR
    -> IR.NetMapIR
    -> SimpleAst.ModuleDeclaration
    -> IR.PortDeclarationMapIR
generatePortDeclarationIRs variables nets moduleDeclaration =
    let portDeclarationList = Maybe.fromMaybe [] $ moduleDeclaration.moduleHeader.portDeclarations
    in Map.fromList $ fmap (generatePortDeclarationIR variables nets) portDeclarationList

generateSubmoduleConnections ::
    IR.VariableMapIR
    -> IR.NetMapIR
    -> SimpleAst.PortConnection
    -> (IR.PortIdentifierIR, IR.ExpressionIR)
generateSubmoduleConnections variables nets portConnection =
    (generatePortIdentifier portConnection.portIdentifier, generateExpression variables nets portConnection.expression)


generateSubmoduleIR ::
    IR.VariableMapIR
    -> IR.NetMapIR
    -> SimpleAst.ModuleInstantiation
    -> IR.SubmoduleIR
generateSubmoduleIR variables nets moduleInstantiation =
    IR.SubmoduleIR {
        submoduleIdentifier = generateModuleIdentifier moduleInstantiation.moduleIdentifier,
        submoduleInstanceIdentifier = generateModuleInstanceIdentifier moduleInstantiation.hierarchicalInstance.instanceIdentifier,
        connections = Map.fromList $ generateSubmoduleConnections variables nets <$> moduleInstantiation.hierarchicalInstance.portConnections
    }

generateSubmoduleIRs ::
    IR.VariableMapIR
    -> IR.NetMapIR
    -> SimpleAst.ModuleDeclaration
    -> [IR.SubmoduleIR]
generateSubmoduleIRs variables nets moduleDeclaration =
    generateSubmoduleIR variables nets <$> toListOf (types @SimpleAst.ModuleInstantiation) moduleDeclaration


generateModuleIR ::
    SimpleAst.ModuleDeclaration
    -> IR.IR
generateModuleIR moduleDeclaration =
    let variables = generateVariableIRs moduleDeclaration
        nets = generateNetIRs moduleDeclaration
        alwaysConstructs = generateAlwaysConstructIRs variables nets moduleDeclaration
        portsTable = generatePortDeclarationIRs variables nets moduleDeclaration

    in IR.IR {
        moduleIdentifier = generateModuleIdentifier moduleDeclaration.moduleHeader.moduleIdentifier,
        alwaysConstructs = Map.elems alwaysConstructs,
        variables = Map.elems variables,
        nets = [],
        ports = Map.elems portsTable,
        submodules = generateSubmoduleIRs variables nets moduleDeclaration
    }

generateIR ::
    SimpleAst.SourceText
    -> Either String [IR.IR]
generateIR sourceText =
    Right $ generateModuleIR <$> toListOf (types @SimpleAst.ModuleDeclaration) sourceText

unionIRExpanded :: IR.ExpandedIR -> IR.ExpandedIR -> IR.ExpandedIR
unionIRExpanded a b =
    IR.ExpandedIR {
        alwaysConstructs = Map.union a.alwaysConstructs b.alwaysConstructs,
        variables = Map.union a.variables b.variables,
        nets = Map.union a.nets b.nets,
        connections = Map.unionWith (++) a.connections b.connections
    }

connect ::
    IR.SubmoduleIR
    -> (forall a . (Ord a, Show a) => IR.HierarchicalIdentifierIR a -> IR.HierarchicalIdentifierIR a)
    -> (forall a . (Ord a, Show a) => IR.HierarchicalIdentifierIR a -> IR.HierarchicalIdentifierIR a)
    -> IR.PortDeclarationIR
    -> Maybe (IR.HierarchicalIdentifierIR IR.VariableOrNetIdentifierIR, [(Maybe IR.SelectIR, IR.ConnectionIR)])
connect submodule superIdentifierGenerator subIdentifierGenerator (IR.PortDeclarationIR portIdentifier IR.PDInput pConn) =
    case (Map.!) submodule.connections portIdentifier of
        IR.EConnection conn -> Just (superIdentifierGenerator $ IR.fetchHierarchicalIdentifierFromConnection conn, [(IR.fetchMaybeSelectIRFromConnection conn, updateConnectionIRHierarchicalIdentifierIRs subIdentifierGenerator pConn)])
        _ -> Nothing
connect submodule superIdentifierGenerator subIdentifierGenerator (IR.PortDeclarationIR portIdentifier IR.PDOutput pConn) =
    case (Map.!) submodule.connections portIdentifier of
        IR.EConnection conn -> Just (subIdentifierGenerator $ IR.fetchHierarchicalIdentifierFromConnection pConn, [(Nothing, updateConnectionIRHierarchicalIdentifierIRs superIdentifierGenerator conn)])
        _ -> Nothing

addIRsAndConnect ::
    [IR.IR]
    -> IR.IR
    -> (forall a . (Ord a, Show a) => IR.HierarchicalIdentifierIR a -> IR.HierarchicalIdentifierIR a)
    -> IR.ExpandedIR
    -> IR.SubmoduleIR
    -> IR.ExpandedIR
addIRsAndConnect irs ir identifier expanded submodule =
    let submoduleAsIR = Maybe.fromJust $ IR.findIRFromSubmodule irs submodule
        expandedSub = generateExpandedIRSubmodule irs submoduleAsIR (identifier . IR.H submodule.submoduleInstanceIdentifier)
        conns = Map.fromList $ Maybe.mapMaybe (connect submodule identifier (identifier . IR.H submodule.submoduleInstanceIdentifier)) (submoduleAsIR.ports)
    in unionIRExpanded expanded $ IR.ExpandedIR {
        alwaysConstructs=expandedSub.alwaysConstructs,
        variables = expandedSub.variables,
        nets = expandedSub.nets,
        connections = Map.unionWith (++) expandedSub.connections conns
        }

updateConnectionIRHierarchicalIdentifierIRs ::
    (forall a . (Ord a, Show a) => IR.HierarchicalIdentifierIR a -> IR.HierarchicalIdentifierIR a)
    -> IR.ConnectionIR
    -> IR.ConnectionIR
updateConnectionIRHierarchicalIdentifierIRs idenGen (IR.ConnectionVariableIR h s) = IR.ConnectionNetIR (idenGen h) s
updateConnectionIRHierarchicalIdentifierIRs idenGen (IR.ConnectionNetIR h s) = IR.ConnectionNetIR (idenGen h) s

updateExpressionIRHierarchicalIdentifiersIRs ::
    (forall a . (Ord a, Show a) => IR.HierarchicalIdentifierIR a -> IR.HierarchicalIdentifierIR a)
    -> IR.ExpressionIR
    -> IR.ExpressionIR
updateExpressionIRHierarchicalIdentifiersIRs idenGen (IR.EConnection conn) = IR.EConnection (updateConnectionIRHierarchicalIdentifierIRs idenGen conn)
updateExpressionIRHierarchicalIdentifiersIRs idenGen (IR.EUnaryOperator u e) = IR.EUnaryOperator u (updateExpressionIRHierarchicalIdentifiersIRs idenGen e)
updateExpressionIRHierarchicalIdentifiersIRs _ x = x

updateStatementItemIRHierarchicalIdentifierIRs ::
    (forall a . (Ord a, Show a) => IR.HierarchicalIdentifierIR a -> IR.HierarchicalIdentifierIR a)
    -> IR.StatementItemIR
    -> IR.StatementItemIR
updateStatementItemIRHierarchicalIdentifierIRs idenGen (IR.BlockingAssignment conn expression) =
    IR.BlockingAssignment (updateConnectionIRHierarchicalIdentifierIRs idenGen conn) $ updateExpressionIRHierarchicalIdentifiersIRs idenGen expression
updateStatementItemIRHierarchicalIdentifierIRs idenGen (IR.NonblockingAssignment conn expression) =
    IR.NonblockingAssignment (updateConnectionIRHierarchicalIdentifierIRs idenGen conn) $ updateExpressionIRHierarchicalIdentifiersIRs idenGen expression


updateAlwaysConstructIRHierarchicalIdentifierIRs ::
    (forall a . (Ord a, Show a) => IR.HierarchicalIdentifierIR a -> IR.HierarchicalIdentifierIR a)
    -> IR.AlwaysConstructIR
    -> IR.AlwaysConstructIR
updateAlwaysConstructIRHierarchicalIdentifierIRs idenGen alwaysConstruct =
    over IR.inputConnections (Set.map $ updateConnectionIRHierarchicalIdentifierIRs idenGen) alwaysConstruct
    & over IR.outputConnections (Set.map $ updateConnectionIRHierarchicalIdentifierIRs idenGen)
    & over IR.statementItems (fmap $ updateStatementItemIRHierarchicalIdentifierIRs idenGen)

generateExpandedIRSubmodule ::
    [IR.IR]
    -> IR.IR
    -> (forall a . (Ord a, Show a) => IR.HierarchicalIdentifierIR a -> IR.HierarchicalIdentifierIR a)
    -> IR.ExpandedIR
generateExpandedIRSubmodule irs ir identifier =
    let alwaysConstructs = fmap (\alwaysConstruct -> (identifier $ IR.I alwaysConstruct._alwaysConstructIdentifier, updateAlwaysConstructIRHierarchicalIdentifierIRs identifier alwaysConstruct)) ir.alwaysConstructs
        variables = fmap (\variable -> (identifier $ IR.I variable.identifier, variable)) ir.variables
        nets = fmap (\nets -> (identifier $ IR.I nets.identifier, nets)) ir.nets
        noConnections = IR.ExpandedIR {
                            alwaysConstructs = Map.fromList alwaysConstructs,
                            variables = Map.fromList variables,
                            nets= Map.fromList nets,
                            connections= Map.empty
                        }
    in  foldl (addIRsAndConnect irs ir identifier) noConnections ir.submodules

moduleIdenToModuleInstanceIden (IR.ModuleIdentifierIR iden) = IR.ModuleInstanceIdentifierIR iden

generateExpandedIR ::
    IR.IR
    -> [IR.IR]
    -> IR.ExpandedIR
generateExpandedIR toplevel irs =
    generateExpandedIRSubmodule irs toplevel $ IR.H $ moduleIdenToModuleInstanceIden toplevel.moduleIdentifier