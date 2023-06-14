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
        _ -> traceShow statementItem undefined
updateTimeSlotFromSensitiveProcess :: AlwaysConstructIR -> State TimeSlot ()
updateTimeSlotFromSensitiveProcess alwaysConstruct =
    do
        timeSlotInit <- get
        put $ foldl (\ts si -> addManyToReactiveRegion ts (statementItemToTimeSlotEvent si)) timeSlotInit alwaysConstruct._statementItems
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
        UOLogicalNot -> SignalValue resultType $ unaryOpExclamationMark (toSVDataObject $ evaluateExpression ir resultType circuitState expr) resultType
        UOBitwiseNot -> undefined
        UOAmpersand -> undefined
        UOTildaAmpersand -> undefined
        UOPipe -> undefined
        UOTildePipe -> undefined
        UOCaret -> undefined
        UOTildeCaret -> undefined
        UOCaretTilde -> undefined

evaluateExpression ir resultType circuitState (EBinaryOperator binaryOp expr1 expr2) =
    let expr1DataObject = toSVDataObject $ evaluateExpression ir resultType circuitState expr1
        expr2DataObject = toSVDataObject $ evaluateExpression ir resultType circuitState expr2
    in case binaryOp of
        BOPlus -> SignalValue resultType $ binaryOpPlus expr1DataObject resultType expr2DataObject


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
            updateTimeSlotFromSignalValueUpdate sensitiveProcessMap signalValueChange

    let updatedTimeSlot = execState timeSlotUpdater initialTimeSlot
    -- trace (unpack (pShow toSet) ++ " " ++ unpack (pShow startValue) ++ "---\n" ++ unpack (pShow evaluatedExpr) ++ "\n") $ pure ()
    put (updatedCircuitState, updatedTimeSlot)
    propagateUpdate expandedIr toSet

executeEvent expandedIr (ScheduleNBAUpdate toSet expr) = do
    (circuitState, initialTimeSlot) <- get
    let signalIdentifier = fetchHierarchicalIdentifierFromConnection toSet
    let (SignalValue dataType _) = case Map.lookup signalIdentifier circuitState of
                                        Just a -> a
                                        _ -> undefined
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
    -> StimulatedCircuit (DynamicCircuitState,DynamicCircuitState)
    -> DynamicCircuitState
eval ir stimulatedCircuit@StimulatedCircuit{_stimulatedState=(start,end), _stimulatedSignals=sigs} =
    let timeSlot = generateTimeSlotFromStimulatedCircuit ir $ StimulatedCircuit end sigs
    in  (start, timeSlot)
            & execState (executeTimeSlot ir)
            & fst
