{-# LANGUAGE DataKinds, TemplateHaskell #-}
module V2H.Simulator.Simulate where
import Control.Monad
import Control.Lens
import Control.Monad.State.Strict
import Data.Set qualified as Set
import Data.Map qualified as Map
import Data.Sequence.Queue as Queue
import Data.Foldable
import Data.Maybe

import Data.Bits
import V2H.IR
import V2H.Simulator.TimeSlot
import GHC.TypeLits
import GHC.Integer
import V2H.IR.DataTypes
import V2H.Simulator.Signal
import V2H.Simulator.Circuit
import V2H.Simple.IRGenerator.Expressions qualified as IR
import Debug.Trace
import Text.Pretty.Simple
import Data.Text.Lazy (unpack)
import qualified V2H.IR as IR
import qualified V2H.IR.DataTypes as IR
import V2H.Simple.IRGenerator.Expressions (mkSignalValueDataObjectFromInteger)

data SignalValueChange = SignalValueChange {
    connection :: ConnectionIR,
    old :: SignalValue,
    new :: SignalValue
} deriving Show

fetchConnectionFromSignalValueChange (SignalValueChange{connection=conn})
    = conn

signalChangeToDynamicEnd signalChange =
    IR.SignalValue (fetchSignalDataType signalChange) $ IR.mkSignalValueDataObjectFromInteger signalChange._end.value

signalChangeToDynamicStart signalChange =
    IR.SignalValue (fetchSignalDataType signalChange) $ IR.mkSignalValueDataObjectFromInteger signalChange._start.value

class FetchHierarchicalIdentifierIR s where
    fetchHierarchicalIdentifierIR :: s -> HierarchicalIdentifierIR VariableOrNetIdentifierIR
    fetchSignalDataType :: s -> DataTypeIR
-- updateSignals :: IR -> (moduleSignals, Set.Set ModuleItemIdentifierIR) -> TimeSlot
-- updateSignals ir (moduleSignals, updateSet) =


-- executeRegion :: IR -> State (circuitState, TimeSlot) ()
-- executeRegion =
--     undefined

-- execute
-- executeActiveRegion = undefined
-- execute

data StimulatedCircuit a =
    StimulatedCircuit {
        _stimulatedState :: a,
        _stimulatedSignals :: Set.Set (HierarchicalIdentifierIR VariableOrNetIdentifierIR)
    }  deriving (Show)
$(makeLenses ''StimulatedCircuit)
mkStimulatedCircuit initialState = StimulatedCircuit initialState Set.empty

executeTimeSlot ::
    ExpandedIR
    -> State (DynamicCircuitState,TimeSlot) ()
executeTimeSlot ir = do
    executeActiveRegionSet ir
    executeReActiveRegionSet ir
    emptyTimeSlot <- isTimeSlotEmpty
    unless emptyTimeSlot $ executeTimeSlot ir

statementItemToTimeSlotEvent :: StatementItemIR -> [TimeSlotEvent]
statementItemToTimeSlotEvent statementItem =
    case statementItem of
            NonblockingAssignment connection expression -> singleton $ ScheduleNBAUpdate connection expression
            BlockingAssignment connection expression -> singleton $ UpdateNetVariable connection expression
            SeqBlock statements -> concatMap statementItemToTimeSlotEvent statements
            ConditionalStatement conditionalStatement ->
                let ifAndElseIfBranches = (fmap . fmap) statementItemToTimeSlotEvent conditionalStatement.ifAndElseIfBranches
                    elseBranch = fmap statementItemToTimeSlotEvent conditionalStatement.elseBranch
                in pure (ConditialTimeSlotEvent ifAndElseIfBranches elseBranch)
updateTimeSlotFromSensitiveProcess :: AlwaysConstructIR -> State TimeSlot ()
updateTimeSlotFromSensitiveProcess alwaysConstruct =
    do
        timeSlotInit <- get
        put $ foldl (\ts si -> addManyToReactiveRegion ts (statementItemToTimeSlotEvent si)) timeSlotInit alwaysConstruct._statementItems
        return ()


-- Should throw some exception when data types don't allign?
doesTriggerLevelChange (SignalValueChange {old=(SignalValue dt vOld), new=(SignalValue _ vNew), connection=(ConnectionVariableIR h1 Nothing)}) (ConnectionVariableIR h2 Nothing)
    = (h1==h2) && isFalse (binaryEqualEqual vOld dt vNew)
doesTriggerLevelChange (SignalValueChange {old=(SignalValue dt vOld), new=(SignalValue _ vNew), connection=(ConnectionNetIR h1 Nothing)}) (ConnectionNetIR h2 Nothing)
    = (h1==h2) && isFalse (binaryEqualEqual vOld dt vNew)
doesTriggerPosedge (SignalValueChange {old=(SignalValue _ vOld), new=(SignalValue _ vNew), connection=(ConnectionVariableIR h1 Nothing)}) (ConnectionVariableIR h2 Nothing) =
    (h1 == h2 )
    && case (getLSB vOld, getLSB vNew) of
        (False,True) -> True
        _ -> False
doesTriggerPosedge (SignalValueChange {old=(SignalValue _ vOld), new=(SignalValue _ vNew), connection=(ConnectionNetIR h1 Nothing)}) (ConnectionNetIR h2 Nothing) =
    (h1 == h2 )
    && case (getLSB vOld, getLSB vNew) of
            (False,True) -> True
            _ -> False

doesTriggerNegedge (SignalValueChange{old=(SignalValue _ vOld), new=(SignalValue _ vNew), connection=(ConnectionVariableIR h1 Nothing)}) (ConnectionVariableIR h2 Nothing) =
    (h1 == h2)
    && case (getLSB vOld, getLSB vNew) of
            (True,False) -> True
            _ -> False
doesTriggerNegedge (SignalValueChange{old=(SignalValue _ vOld), new=(SignalValue _ vNew), connection=(ConnectionNetIR h1 Nothing)}) (ConnectionNetIR h2 Nothing) =
    (h1 == h2)
    && case (getLSB vOld, getLSB vNew) of
            (True,False) -> True
            _ -> False

doesSignalChangeTriggerEvent ::SignalValueChange -> EventExpressionIR -> Bool
doesSignalChangeTriggerEvent signalValueChange eventExpression =
    case eventExpression.edgeIdentifier of
        Just Posedge -> doesTriggerPosedge signalValueChange eventExpression.connection
        Just Negedge -> doesTriggerNegedge signalValueChange eventExpression.connection
        Just Edge -> doesTriggerLevelChange signalValueChange eventExpression.connection
        Nothing ->  doesTriggerLevelChange signalValueChange eventExpression.connection

doesSignalChangeTriggerProcess :: SignalValueChange -> AlwaysConstructIR -> Bool
doesSignalChangeTriggerProcess signalValueChange sensitiveProcess =
    let triggerslevelChange =
            any (doesTriggerLevelChange signalValueChange) sensitiveProcess._inputConnections
        triggersEventExpressions =
            any (doesSignalChangeTriggerEvent signalValueChange)
    in case sensitiveProcess._sensitivity of
            Comb -> triggerslevelChange
            FF eventExpressions -> triggersEventExpressions eventExpressions
            Latch -> triggerslevelChange

updateTimeSlotFromSignalValueUpdate ::
    Map.Map (HierarchicalIdentifierIR VariableOrNetIdentifierIR) (Set.Set AlwaysConstructIR)
    -> SignalValueChange
    -> State TimeSlot ()
updateTimeSlotFromSignalValueUpdate sensitiveProcessMap signalValueChange =
    do
        timeslot <- get
        let updateTimeSlotWhenSignalChangeTriggerProcess elem =
                when (doesSignalChangeTriggerProcess signalValueChange elem) $ updateTimeSlotFromSensitiveProcess elem
        let changingSignal = fetchHierarchicalIdentifierFromConnection . fetchConnectionFromSignalValueChange $ signalValueChange
        case Map.lookup changingSignal sensitiveProcessMap of
            Nothing -> return ()
            Just sensitiveProcesses ->
                mapM_ updateTimeSlotWhenSignalChangeTriggerProcess $ Set.toList sensitiveProcesses

generateTimeSlotEventWithIdentifier ::
        DynamicCircuitState
        -> HierarchicalIdentifierIR VariableOrNetIdentifierIR
        -> TimeSlotEvent
generateTimeSlotEventWithIdentifier stimulatedCircuit sigIdentifier =
    let newVal = case Map.lookup sigIdentifier stimulatedCircuit of
                    Just a -> a
                    _ -> trace (unpack $ pShow stimulatedCircuit) $ traceShow sigIdentifier undefined
    in ScheduleNBAUpdate (ConnectionVariableIR sigIdentifier Nothing) $ ELiteral newVal

generateTimeSlotFromStimulatedCircuit ::
    ExpandedIR
    -> StimulatedCircuit DynamicCircuitState
    -> TimeSlot
generateTimeSlotFromStimulatedCircuit ir (StimulatedCircuit stimState stimSignals) =
    foldl
        (\ts iden ->
            addToReactiveRegion ts (generateTimeSlotEventWithIdentifier stimState iden)
        ) emptyTimeSlot stimSignals

getValueFromConnection :: DynamicCircuitState -> ConnectionIR -> SignalValue
getValueFromConnection dynamicCircuitState (ConnectionVariableIR h Nothing) =
     case Map.lookup h dynamicCircuitState of
        Just a -> a
        _ -> undefined
getValueFromConnection dynamicCircuitState (ConnectionNetIR h Nothing) =
     case Map.lookup h dynamicCircuitState of
        Just a -> a
        _ -> undefined
data ExpressionType = SelfDetermined | ContextDetermined DataTypeIR deriving (Show)
whenSD SelfDetermined dt = dt
whenSD (ContextDetermined dt) _ = dt

chooseWith f (a,b) =
    if f a > f b then a
    else b
getDataTypeOfSelfDeterminedExpression ::
    IR.ExpandedIR
    -> IR.ExpressionIR
    -> IR.DataTypeIR
getDataTypeOfSelfDeterminedExpression expandedIR expr =
    case expr of
        (ELiteral (SignalValue dt _)) -> dt
        (EConnection (ConnectionVariableIR hn Nothing) ) -> (expandedIR.variables Map.! hn).dataType
        (EUnaryOperator _ _) -> DTSingular $ STScalar SIVTLogic
        (EBinaryOperator bop expr1 expr2)
            ->  let maxSubExpr = (getDataTypeOfSelfDeterminedExpression expandedIR expr1, getDataTypeOfSelfDeterminedExpression expandedIR expr2)
                                    & chooseWith IR.getBitWidth
                in case bop of
                        BOPlus -> maxSubExpr
                        BOMinus -> maxSubExpr
                        BOAsterisk -> maxSubExpr
                        BOForwardSlash -> maxSubExpr
                        BOEqualEqual -> DTSingular $ STScalar SIVTLogic

evaluateExpression ::
    ExpandedIR
    -> ExpressionType
    -> DynamicCircuitState
    -> ExpressionIR
    -> SignalValue
evaluateExpression _ _ _ (ELiteral x) = x
evaluateExpression _ (ContextDetermined dt) state (EConnection conn) =
    cast (toSVDataObject $ getValueFromConnection state conn) dt
evaluateExpression _ SelfDetermined state (EConnection conn) =
    getValueFromConnection state conn
evaluateExpression ir expressionType circuitState (EUnaryOperator unaryOp expr) =
    let oneBitResultType = whenSD expressionType $ DTSingular (STScalar SIVTLogic)
    in case unaryOp of
        UOPlus -> evaluateExpression ir expressionType circuitState expr
        UOMinus ->
            SignalValue oneBitResultType $ unaryOpMinus (toSVDataObject $ evaluateExpression ir expressionType circuitState expr) oneBitResultType
        UOLogicalNot ->
            SignalValue oneBitResultType $ unaryOpExclamationMark (toSVDataObject $ evaluateExpression ir expressionType circuitState expr) oneBitResultType
        UOBitwiseNot -> undefined
        UOAmpersand -> undefined
        UOTildaAmpersand -> undefined
        UOPipe -> undefined
        UOTildePipe -> undefined
        UOCaret -> undefined
        UOTildeCaret -> undefined
        UOCaretTilde -> undefined

evaluateExpression ir expressionType circuitState e@(EConcat exprs) =
    let dataTypes =  getDataTypeOfSelfDeterminedExpression ir <$> exprs
        values = fmap (toSVDataObject . evaluateExpression ir expressionType circuitState) exprs
        valuesAndDataTypes = zip values dataTypes
        concatSVDataObjects (svdo1, dt1) (svdo2, dt2) = concatOp svdo1 dt1 dt2 svdo2
        (resultObject, resultType) = foldl concatSVDataObjects (mkSignalValueDataObjectFromInteger 0, DTUnit) valuesAndDataTypes
    in  cast resultObject (whenSD expressionType resultType)
evaluateExpression ir expressionType circuitState e@(EBinaryOperator binaryOp expr1 expr2) =
    let resultType = whenSD expressionType $ getDataTypeOfSelfDeterminedExpression ir e
        resultTypeLeft = whenSD expressionType $ getDataTypeOfSelfDeterminedExpression ir expr1
        resultTypeRight = whenSD expressionType $ getDataTypeOfSelfDeterminedExpression ir expr2
        largestBitWidthOfInnerTypes = chooseWith getBitWidth (resultTypeLeft, resultTypeRight)
        expr1DataObject = toSVDataObject $ evaluateExpression ir expressionType circuitState expr1
        expr2DataObject = toSVDataObject $ evaluateExpression ir expressionType circuitState expr2

    in case binaryOp of
        BOPlus -> cast (binaryOpPlus expr1DataObject largestBitWidthOfInnerTypes expr2DataObject) resultType
        BOEqualEqual -> cast (binaryEqualEqual expr1DataObject largestBitWidthOfInnerTypes expr2DataObject) (DTSingular (STScalar SIVTLogic))
        BOAsterisk -> cast (binaryAsterisk expr1DataObject largestBitWidthOfInnerTypes expr2DataObject) resultType
        BOMinus -> cast (binaryMinus expr1DataObject largestBitWidthOfInnerTypes expr2DataObject) resultType

generateUpdateNetVariableFromConnection ::
    DynamicCircuitState
    -> HierarchicalIdentifierIR VariableOrNetIdentifierIR
    -> (Maybe SelectIR, ConnectionIR)
    -> TimeSlotEvent
generateUpdateNetVariableFromConnection circuitState iden (Nothing, conn) =
    case conn of
        ConnectionVariableIR h1 Nothing ->
            let signalValue = case Map.lookup iden circuitState of
                                    Just a -> a
                                    _ -> undefined
            in UpdateNetVariable conn $ ELiteral signalValue
        ConnectionNetIR h1 Nothing ->
            let signalValue = case Map.lookup iden circuitState of
                                    Just a -> a
                                    _ -> undefined
            in UpdateNetVariable conn $ ELiteral signalValue
        _ -> traceShow conn undefined

propagateUpdate ::
    ExpandedIR
    -> ConnectionIR
    -> State (DynamicCircuitState, TimeSlot) ()
propagateUpdate expandedIR conn =
        let propagate changedSignal =
                do
                    (circuitState, _) <- get
                    Map.lookup changedSignal expandedIR.connections
                        & concatMap (fmap (generateUpdateNetVariableFromConnection circuitState changedSignal))
                        & traverse_ (executeEvent expandedIR)
        in case conn of
            (ConnectionVariableIR changedSignal Nothing) -> propagate changedSignal
            (ConnectionNetIR changedSignal Nothing) -> propagate changedSignal

executeEvent ::
    ExpandedIR
    -> TimeSlotEvent
    -> State (DynamicCircuitState, TimeSlot) ()
executeEvent expandedIr x@(UpdateNetVariable toSet expr) = do
    -- traceShow toSet $ pure ()
    -- traceShow expr $ pure ()
    (initialCircuitState, initialTimeSlot) <- get
    let signalIdentifier = fetchHierarchicalIdentifierFromConnection toSet
    let startValue@(SignalValue dataType _) = initialCircuitState Map.! signalIdentifier
    let evaluatedExpr = evaluateExpression expandedIr (ContextDetermined dataType) initialCircuitState expr
    let updatedCircuitState = Map.insert signalIdentifier evaluatedExpr initialCircuitState
    let sensitiveProcessMap = mkSensitiveProcessesMapFromIR expandedIr
    let signalValueChange =
            SignalValueChange{
                connection = toSet,
                old = startValue,
                new = evaluatedExpr
            }
    let timeSlotUpdater =
            updateTimeSlotFromSignalValueUpdate sensitiveProcessMap signalValueChange

    let updatedTimeSlot = execState timeSlotUpdater initialTimeSlot
    put (updatedCircuitState, updatedTimeSlot)
    propagateUpdate expandedIr toSet

executeEvent expandedIr e@(ScheduleNBAUpdate toSet expr) = do
    (circuitState, initialTimeSlot) <- get
    let signalIdentifier = fetchHierarchicalIdentifierFromConnection toSet
    let (SignalValue dataType _) = case Map.lookup signalIdentifier circuitState of
                                        Just a -> a
                                        _ -> undefined
    let evaluatedExpr = evaluateExpression expandedIr (ContextDetermined dataType) circuitState expr
    let updatedTimeSlot = addToNbaRegion initialTimeSlot $ UpdateNetVariable toSet $ ELiteral evaluatedExpr
    put (circuitState, updatedTimeSlot)

executeEvent expandedIR (ConditialTimeSlotEvent ifAndElseIfBranches elseBranch) = do
    (circuitState, initialTimeSlot) <- get
    let branchConditionTrue (expression,_) = isTrue $ toSVDataObject $ evaluateExpression expandedIR SelfDetermined circuitState expression
    let timeSlotEventsToAdd = maybe (concat elseBranch) snd (find branchConditionTrue ifAndElseIfBranches)
    let updatedTimeSlot = addManyToNbaRegion initialTimeSlot timeSlotEventsToAdd
    put (circuitState, updatedTimeSlot)

executeEvents ::
    ExpandedIR
    -> Queue TimeSlotEvent
    -> State (DynamicCircuitState, TimeSlot) ()
executeEvents ir = mapM_ $ executeEvent ir

executeActiveRegionSet ::
    ExpandedIR
    -> State (DynamicCircuitState, TimeSlot) ()
executeActiveRegionSet ir = do
    maybeEvents <- popFirstRegionInActiveRegionSet
    case maybeEvents of
        Nothing -> return ()
        Just e -> do
                    executeEvents ir e
                    executeActiveRegionSet ir

executeReActiveRegionSet ::
    ExpandedIR
    -> State (DynamicCircuitState, TimeSlot) ()
executeReActiveRegionSet ir = do
    events <- popFirstRegionInReactiveRegionSet
    case events of
        Just e -> do
                    executeEvents ir e
                    executeReActiveRegionSet ir
        Nothing -> return ()

eval ::
    ExpandedIR
    -> StimulatedCircuit (DynamicCircuitState,DynamicCircuitState)
    -> DynamicCircuitState
eval ir stimulatedCircuit@StimulatedCircuit{_stimulatedState=(start,end), _stimulatedSignals=sigs} =
    let timeSlot = generateTimeSlotFromStimulatedCircuit ir $ StimulatedCircuit end sigs
    in  (start, timeSlot)
            & execState (executeTimeSlot ir)
            & fst

triggerAllAlwaysCombBlocks :: ExpandedIR -> DynamicCircuitState -> DynamicCircuitState
triggerAllAlwaysCombBlocks expandedIR dynamicStart = do
    let alwaysCombConstructs = Map.elems $ Map.filter (\a -> a._sensitivity == Comb) expandedIR.alwaysConstructs
        timeSlot' = traverse_ updateTimeSlotFromSensitiveProcess alwaysCombConstructs
                    & flip execState emptyTimeSlot

    fst $ execState (executeTimeSlot expandedIR) (dynamicStart, timeSlot')
