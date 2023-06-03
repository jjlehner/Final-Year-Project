{-# LANGUAGE DataKinds, TemplateHaskell #-}
module V2H.Simulator.Simulate where
import Control.Monad
import Control.Lens
import Control.Monad.State.Strict
import Data.Set qualified as Set
import Data.Map qualified as Map
import Data.Sequence.Queue as Queue
import Data.Maybe

import Data.Bits
import V2H.IR
import V2H.IR.DataTypes
import V2H.Simulator.TimeSlot
import GHC.TypeLits
import GHC.Integer
import V2H.IR.DataTypes (DataTypeIR)
import V2H.Simulator.SignalDynamic
import Debug.Trace


class FindNetVariableIdentifier s where
    fetchNetVariableIdentifier :: s -> VariableOrNetIdentifierIR

-- updateSignals :: IR -> (moduleSignals, Set.Set ModuleItemIdentifierIR) -> TimeSlot
-- updateSignals ir (moduleSignals, updateSet) =


-- executeRegion :: IR -> State (circuitState, TimeSlot) ()
-- executeRegion =
--     undefined

-- execute
-- executeActiveRegion = undefined
-- execute

getFirstRegionInActiveRegionSet :: State (circuitState, TimeSlot) (Maybe (Queue.Queue TimeSlotEvent))
getFirstRegionInActiveRegionSet = do
    (t, ts) <- get
    let emptyActiveRegion = do
            put (t, ts {_activeRegion = Queue.empty})
            return $ Just ts._activeRegion
    let emptyNbaRegion = do
            put (t, ts {_nbaRegion = Queue.empty})
            return $ Just  ts._nbaRegion

    if ts._activeRegion /= Queue.empty then emptyActiveRegion
    else if ts._nbaRegion /= Queue.empty then emptyNbaRegion
    else return Nothing

getFirstRegionInReactiveRegionSet :: State (circuitState, TimeSlot) (Maybe (Queue.Queue TimeSlotEvent))
getFirstRegionInReactiveRegionSet = do
    (t, ts) <- get
    let emptyReactiveRegion = do
            put (t, ts {_reActiveRegion = Queue.empty})
            return $ Just ts._reActiveRegion
    let emptyReNbaRegion = do
            put (t, ts {_reNbaRegion = Queue.empty})
            return $ Just  ts._reNbaRegion
    if ts._reActiveRegion /= Queue.empty then emptyReactiveRegion
    else if ts._reNbaRegion /= Queue.empty then emptyReNbaRegion
    else return Nothing

isTimeSlotEmpty :: State (circuitState,TimeSlot) Bool
isTimeSlotEmpty = do
    (_, ts) <- get
    return (ts._activeRegion == Queue.empty
            && ts._nbaRegion == Queue.empty
            && ts._reActiveRegion == Queue.empty
            && ts._reNbaRegion == Queue.empty)

data StimulatedCircuit a =
    StimulatedCircuit {
        _stimulatedState :: a,
        _stimulatedSignals :: Set.Set VariableOrNetIdentifierIR
    }  deriving (Show)
$(makeLenses ''StimulatedCircuit)
mkStimulatedCircuit initialState = StimulatedCircuit initialState Set.empty

executeTimeSlot :: (Show circuitState) =>
    IR
    -> Map.Map VariableOrNetIdentifierIR (ReifiedLens' circuitState SignalDynamic)
    -> State (circuitState,TimeSlot) ()
executeTimeSlot ir lensMap = do
    executeActiveRegionSet ir lensMap
    executeReActiveRegionSet ir lensMap
    emptyTimeSlot <- isTimeSlotEmpty
    unless emptyTimeSlot $ executeTimeSlot ir lensMap

statementItemToTimeSlotEvent :: StatementItemIR -> TimeSlotEvent
statementItemToTimeSlotEvent statementItem =
    case statementItem of
        NonblockingAssignment connection expression -> ScheduleNBAUpdate connection expression
        BlockingAssignment connection expression -> UpdateNetVariable connection expression

updateTimeSlotFromSensitiveProcess :: AlwaysConstructIR -> State TimeSlot ()
updateTimeSlotFromSensitiveProcess alwaysConstruct =
    do
        timeSlotInit <- get
        put $ foldl (\ts si -> addToReactiveRegion ts (statementItemToTimeSlotEvent si)) timeSlotInit alwaysConstruct.statementItems
        return ()

isConnectionSignalChangeTriggerLevel (SignalDynamic _ vOld) (SignalDynamic _ vNew) (ConnectionVariableIR variableIR Nothing) = vOld /= vNew

isConnectionSignalChangeTriggerPosedge (SignalDynamic _ (SignalValue _ vOld)) (SignalDynamic _ (SignalValue _ vNew)) (ConnectionVariableIR variableIR Nothing)  =
    case (getLSB vOld, getLSB vNew) of
        (True,False) -> True
        _ -> False

isConnectionSignalChangeTriggerNegedge (SignalDynamic _ (SignalValue _ vOld)) (SignalDynamic _ (SignalValue _ vNew)) (ConnectionVariableIR variableIR Nothing) =
    case (getLSB vOld, getLSB vNew) of
        (True,False) -> True
        _ -> False

doesSignalChangeTriggerEvent ::SignalDynamic -> SignalDynamic -> EventExpressionIR -> Bool
doesSignalChangeTriggerEvent sigOld sigNew eventExpression =
    let edgeDetectionMethod = case eventExpression.edgeIdentifier of
                                    Just Posedge -> isConnectionSignalChangeTriggerPosedge
                                    Just Negedge -> isConnectionSignalChangeTriggerNegedge
                                    Just Edge -> isConnectionSignalChangeTriggerLevel
                                    Nothing ->  isConnectionSignalChangeTriggerLevel
        matchedSignal = sigOld.signalIdentifier == fetchNetVariableIdentifierFromConnection eventExpression.connection
    in matchedSignal || edgeDetectionMethod sigOld sigNew eventExpression.connection

doesSignalChangeTriggerProcess :: SignalDynamic -> SignalDynamic -> AlwaysConstructIR -> Bool
doesSignalChangeTriggerProcess sigOld sigNew alwaysConstruct =
    case alwaysConstruct.sensitivity of
        Comb -> any (isConnectionSignalChangeTriggerLevel sigOld sigNew) alwaysConstruct.inputConnections
        FF eventExpressions -> any (doesSignalChangeTriggerEvent sigOld sigNew) eventExpressions
        Latch -> any (isConnectionSignalChangeTriggerLevel sigOld sigNew) alwaysConstruct.inputConnections

generateTimeSlotFromTestbenchSignalUpdate ::
    Map.Map VariableOrNetIdentifierIR (Set.Set AlwaysConstructIR)
    -> Map.Map VariableOrNetIdentifierIR (ReifiedLens' circuitState SignalDynamic)
    -> circuitState
    -> circuitState
    -> VariableOrNetIdentifierIR
    -> State TimeSlot ()
generateTimeSlotFromTestbenchSignalUpdate sensitiveProcessMap lensMap oldState testbenchState variableOrNetIdentifier =
    do
        timeslot <- get
        let lens = lensMap Map.! variableOrNetIdentifier
        let oldSig = view (runLens lens) oldState
        let newSig = view (runLens lens) testbenchState
        let updateTimeSlotwhenSignalChangeTriggerProcess elem = when (doesSignalChangeTriggerProcess oldSig newSig elem) $ updateTimeSlotFromSensitiveProcess elem
        case Map.lookup variableOrNetIdentifier sensitiveProcessMap of
            Nothing -> return ()
            Just sensitiveProcesses -> mapM_ updateTimeSlotwhenSignalChangeTriggerProcess $ Set.toList sensitiveProcesses

generateTimeSlotEventWithIdentifier ::
        IR
        -> Map.Map VariableOrNetIdentifierIR (ReifiedLens' circuitState SignalDynamic)
        -> circuitState
        -> VariableOrNetIdentifierIR
        -> TimeSlotEvent
generateTimeSlotEventWithIdentifier ir lensMap stimulatedCircuit sigIdentifier =
    let ln = lensMap Map.! sigIdentifier
        newVal = view (runLens ln) stimulatedCircuit
        updatingVariable = ir.variables Map.! sigIdentifier
    in ScheduleNBAUpdate (ConnectionVariableIR updatingVariable Nothing) $ ELiteral newVal.signalValue
generateTimeSlotFromTestbench ::
    IR
    -> Map.Map VariableOrNetIdentifierIR (ReifiedLens' circuitState SignalDynamic)
    -> StimulatedCircuit circuitState
    -> TimeSlot
generateTimeSlotFromTestbench ir lensMap (StimulatedCircuit stimState stimSignals) =
    foldl (\ts iden -> addToReactiveRegion ts (generateTimeSlotEventWithIdentifier ir lensMap stimState iden)) emptyTimeSlot stimSignals

evaluateExpression ::
    IR
    -> Map.Map VariableOrNetIdentifierIR (ReifiedLens' circuitState SignalDynamic)
    -> DataTypeIR
    -> circuitState
    -> ExpressionIR
    -> SignalValue
evaluateExpression _ _ resultType circuitState (ELiteral x) = x
evaluateExpression ir lensMap resultType circuitState (EConnection conn) =
    let fetchLens = lensMap Map.! fetchNetVariableIdentifierFromConnection conn
    in mkSignalValueFromSignalDynamic resultType $ view (runLens fetchLens) circuitState
        -- & traceShow (view (runLens fetchLens) circuitState)

evaluateExpression ir lensMap resultType circuitState (EUnaryOperator unaryOp expr) =
    traceShow (evaluateExpression ir lensMap resultType circuitState expr) $
    case unaryOp of
        UOPlus -> evaluateExpression ir lensMap resultType circuitState expr
        UOMinus ->  SignalValue resultType $ unaryOpMinus (toSVDataObject $ evaluateExpression ir lensMap resultType circuitState expr) resultType
        UOExclamationMark -> SignalValue resultType $ unaryOpExclamationMark (toSVDataObject $ evaluateExpression ir lensMap resultType circuitState expr) resultType
        UOTilda -> undefined
        UOAmpersand -> undefined
        UOTildaAmpersand -> undefined
        UOPipe -> undefined
        UOTildePipe -> undefined
        UOCaret -> undefined
        UOTildeCaret -> undefined
        UOCaretTilde -> undefined

executeEvent :: (Show circuitState) => IR
                -> Map.Map VariableOrNetIdentifierIR (ReifiedLens' circuitState SignalDynamic)
                -> TimeSlotEvent
                -> State (circuitState, TimeSlot) ()
executeEvent ir lensMap (UpdateNetVariable toSet expr) = do
    (oldState, oldTimeSlot) <- get
    let setLens =
            lensMap Map.! fetchNetVariableIdentifierFromConnection toSet
    let initialVariable@SignalDynamic{signalIdentifier=identifier, signalValue=signalValue} =
            view (runLens setLens) oldState
    let resultDataType =
            signalValue
            & signalValueToDataType

    let evaluatedExpr = SignalDynamic identifier $ evaluateExpression ir lensMap resultDataType oldState expr
    let updatedState = set (runLens setLens) evaluatedExpr oldState
    let sensitiveProcessMap = mkSensitiveProcessesMapFromIR ir
    let timeSlotUpdater = generateTimeSlotFromTestbenchSignalUpdate sensitiveProcessMap lensMap updatedState oldState (fetchNetVariableIdentifierFromConnection toSet)
    let newTimeSlot = execState timeSlotUpdater oldTimeSlot
    put (updatedState, newTimeSlot)

executeEvent ir lensMap (ScheduleNBAUpdate conn expr) = do
    (oldState, oldTimeSlot) <- get
    let toUpdateLens = lensMap Map.! fetchNetVariableIdentifierFromConnection conn
    let resultDataType =
            view (runLens toUpdateLens) oldState
            & signalValue
            & signalValueToDataType
    let evaluatedExpr = evaluateExpression ir lensMap resultDataType oldState expr
    let newTimeSlot = addToNbaRegion oldTimeSlot $ UpdateNetVariable conn $ ELiteral evaluatedExpr
    put (oldState, newTimeSlot)

executeEvents ::(Show circuitState) =>
    IR
    -> Map.Map VariableOrNetIdentifierIR (ReifiedLens' circuitState SignalDynamic)
    -> Queue.Queue TimeSlotEvent
    -> State (circuitState, TimeSlot) ()
executeEvents ir lensMap  = mapM_ $ executeEvent ir lensMap

executeActiveRegionSet ::(Show circuitState) =>
    IR
    -> Map.Map VariableOrNetIdentifierIR (ReifiedLens' circuitState SignalDynamic)
    -> State (circuitState, TimeSlot) ()
executeActiveRegionSet ir lensMap= do
    events <- getFirstRegionInActiveRegionSet
    case events of
        Nothing -> return ()
        Just e -> do
                    executeEvents ir lensMap e
                    executeActiveRegionSet ir lensMap

executeReActiveRegionSet :: (Show circuitState) =>
    IR
    -> Map.Map VariableOrNetIdentifierIR (ReifiedLens' circuitState SignalDynamic)
    -> State (circuitState, TimeSlot) ()
executeReActiveRegionSet ir lensMap = do
    events <- getFirstRegionInReactiveRegionSet
    case events of
        Just e -> do
                    executeEvents ir lensMap e
                    executeReActiveRegionSet ir lensMap
        Nothing -> return ()

eval :: (Show circuitState) =>
    IR
    -> Map.Map VariableOrNetIdentifierIR (ReifiedLens' circuitState SignalDynamic)
    -> State (StimulatedCircuit circuitState) ()
eval ir lensMap = do
    stimulatedCircuit@StimulatedCircuit{_stimulatedState=stimulatedState} <- get
    let timeSlot = generateTimeSlotFromTestbench ir lensMap stimulatedCircuit
    let newState =  (stimulatedState, generateTimeSlotFromTestbench ir lensMap stimulatedCircuit)
                    & execState (executeTimeSlot ir lensMap)
                    & fst
    put $ StimulatedCircuit newState Set.empty