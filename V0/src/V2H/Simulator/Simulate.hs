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
import V2H.Simulator.TimeSlot
import GHC.TypeLits
import GHC.Integer
import V2H.IR.DataTypes
import V2H.Simulator.Signal
import V2H.Simulator.Circuit
import Debug.Trace

data SignalValueChange = SignalValueChange {
    connection :: ConnectionIR,
    old :: SignalValue,
    new :: SignalValue
}

fetchConnectionFromSignalValueChange (SignalValueChange{connection=conn})
    = conn

class FetchHierarchicalIdentifierIR s where
    fetchHierarchicalIdentifierIR :: s -> HierarchicalIdentifierIR VariableOrNetIdentifierIR

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

(<==) :: (FetchHierarchicalIdentifierIR (SignalChange a b))
    => Lens' t (SignalChange a b)
    -> b
    -> State (StimulatedCircuit t) ()
(<==) ln newValue = do
    (StimulatedCircuit circuitState changedSignals) <- get
    let b = set (ln . end) newValue circuitState
    put $ StimulatedCircuit b $ Set.insert (fetchHierarchicalIdentifierIR $ view ln circuitState) changedSignals

executeTimeSlot ::
    ExpandedIR
    -> State (DynamicCircuitState,TimeSlot) ()
executeTimeSlot ir = do
    executeActiveRegionSet ir
    executeReActiveRegionSet ir
    emptyTimeSlot <- isTimeSlotEmpty
    unless emptyTimeSlot $ executeTimeSlot ir

statementItemToTimeSlotEvent :: StatementItemIR -> TimeSlotEvent
statementItemToTimeSlotEvent statementItem =
    case statementItem of
        NonblockingAssignment connection expression -> ScheduleNBAUpdate connection expression
        BlockingAssignment connection expression -> UpdateNetVariable connection expression

updateTimeSlotFromSensitiveProcess :: AlwaysConstructIR -> State TimeSlot ()
updateTimeSlotFromSensitiveProcess alwaysConstruct =
    do
        timeSlotInit <- get
        put $ foldl (\ts si -> addToReactiveRegion ts (statementItemToTimeSlotEvent si)) timeSlotInit alwaysConstruct._statementItems
        return ()

-- Should throw some exception when data types don't allign?
doesTriggerLevelChange (SignalValueChange {old=(SignalValue dt vOld), new=(SignalValue _ vNew), connection=(ConnectionVariableIR h1 Nothing)}) (ConnectionVariableIR h2 Nothing)
    = (h1==h2) && binaryEqualEqual vOld dt vNew
doesTriggerLevelChange (SignalValueChange {old=(SignalValue dt vOld), new=(SignalValue _ vNew), connection=(ConnectionNetIR h1 Nothing)}) (ConnectionNetIR h2 Nothing)
    = (h1==h2) && binaryEqualEqual vOld dt vNew

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
    let newVal = stimulatedCircuit Map.! sigIdentifier
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
    dynamicCircuitState Map.! h

evaluateExpression ::
    ExpandedIR
    -> DataTypeIR
    -> DynamicCircuitState
    -> ExpressionIR
    -> SignalValue
evaluateExpression _ _ _ (ELiteral x) = x
evaluateExpression _ dt state (EConnection conn) =
    SignalValue dt $ toSVDataObject $ getValueFromConnection state conn

evaluateExpression ir resultType circuitState (EUnaryOperator unaryOp expr) =
    case unaryOp of
        UOPlus -> evaluateExpression ir resultType circuitState expr
        UOMinus ->  SignalValue resultType $ unaryOpMinus (toSVDataObject $ evaluateExpression ir resultType circuitState expr) resultType
        UOExclamationMark -> SignalValue resultType $ unaryOpExclamationMark (toSVDataObject $ evaluateExpression ir resultType circuitState expr) resultType
        UOTilda -> undefined
        UOAmpersand -> undefined
        UOTildaAmpersand -> undefined
        UOPipe -> undefined
        UOTildePipe -> undefined
        UOCaret -> undefined
        UOTildeCaret -> undefined
        UOCaretTilde -> undefined

executeEvent ::
    ExpandedIR
    -> TimeSlotEvent
    -> State (DynamicCircuitState, TimeSlot) ()
executeEvent expandedIr (UpdateNetVariable toSet expr) = do
    (initialCircuitState, initialTimeSlot) <- get
    let signalIdentifier = fetchHierarchicalIdentifierFromConnection toSet
    let startValue@(SignalValue dataType _) = initialCircuitState Map.! signalIdentifier
    let evaluatedExpr = evaluateExpression expandedIr dataType initialCircuitState expr
    let updatedCircuitState = Map.insert signalIdentifier evaluatedExpr initialCircuitState
    let sensitiveProcessMap = mkSensitiveProcessesMapFromIR expandedIr

    let signalValueChange =
            SignalValueChange{
                connection = toSet,
                old = startValue,
                new = evaluatedExpr
            }
    let timeSlotUpdater =
            updateTimeSlotFromSignalValueUpdate sensitiveProcessMap  signalValueChange
    let updatedTimeSlot = execState timeSlotUpdater initialTimeSlot
    put (updatedCircuitState, updatedTimeSlot)

executeEvent expandedIr (ScheduleNBAUpdate toSet expr) = do
    (circuitState, initialTimeSlot) <- get
    let signalIdentifier = fetchHierarchicalIdentifierFromConnection toSet
    let (SignalValue dataType _) = circuitState Map.! signalIdentifier
    let evaluatedExpr = evaluateExpression expandedIr dataType circuitState expr
    let updatedTimeSlot = addToNbaRegion initialTimeSlot $ UpdateNetVariable toSet $ ELiteral evaluatedExpr
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
    -> State (StimulatedCircuit DynamicCircuitState) ()
eval ir = do
    stimulatedCircuit@StimulatedCircuit{_stimulatedState=stimulatedState} <- get
    let timeSlot = generateTimeSlotFromStimulatedCircuit ir stimulatedCircuit
    let newState =  (stimulatedState, generateTimeSlotFromStimulatedCircuit ir stimulatedCircuit)
                    & execState (executeTimeSlot ir)
                    & fst
    put $ StimulatedCircuit newState Set.empty