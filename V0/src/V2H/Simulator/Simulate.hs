{-# LANGUAGE DataKinds, TemplateHaskell #-}
module V2H.Simulator.Simulate where
import Control.Monad
import Control.Lens
import Control.Monad.State.Strict
import Data.Function
import Data.Sequence.Queue qualified as Queue
import Data.Set qualified as Set
import Data.Map qualified as Map
import Data.Maybe

import Data.Bits
import V2H.IR
import Debug.Trace
import GHC.TypeLits

newtype Signal sig (a::Nat) = Signal {value::Integer} deriving (Show, Eq)
data SignalDynamic =
    SignalDynamic {
        signalVariableOrNetIdentifier :: VariableOrNetIdentifierIR,
        signalWidth :: Integer,
        signalValue :: Integer
    } deriving (Show)

data TimeSlotEvent = UpdateNetVariable ConnectionIR ExpressionIR | ScheduleNBAUpdate ConnectionIR ExpressionIR deriving (Show, Eq)

class FindNetVariableIdentifier s where
    fetchNetVariableIdentifier :: s -> VariableOrNetIdentifierIR

data TimeSlot =
    TimeSlot {
        _activeRegion :: Queue.Queue TimeSlotEvent,
        _nbaRegion :: Queue.Queue TimeSlotEvent,
        _reActiveRegion :: Queue.Queue TimeSlotEvent,
        _reNbaRegion :: Queue.Queue TimeSlotEvent
    } deriving (Show)


emptyTimeSlot = TimeSlot Queue.empty Queue.empty Queue.empty Queue.empty

$(makeLenses ''TimeSlot)

addToActiveRegion :: TimeSlot -> TimeSlotEvent -> TimeSlot
addToActiveRegion = addToRegion activeRegion
addToNbaRegion :: TimeSlot -> TimeSlotEvent -> TimeSlot
addToNbaRegion = addToRegion nbaRegion
addToReactiveRegion :: TimeSlot -> TimeSlotEvent -> TimeSlot
addToReactiveRegion = addToRegion reActiveRegion
addToReNbaRegion :: TimeSlot -> TimeSlotEvent -> TimeSlot
addToReNbaRegion = addToRegion reNbaRegion

addToRegion regionLens timeSlot event = over regionLens (Queue.|> event) timeSlot
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

data TestbenchCircuitState a =
    TestbenchCircuitState {
        _updatedState :: a,
        _oldState :: a,
        _updatedSignals :: Set.Set VariableOrNetIdentifierIR
    }  deriving (Show)
$(makeLenses ''TestbenchCircuitState)

executeTimeSlot ::
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

isConnectionSignalChangeTriggerLevel (SignalDynamic _ _ vOld) (SignalDynamic _ _ vNew) (ConnectionVariableIR variableIR Nothing) = vOld /= vNew

isConnectionSignalChangeTriggerPosedge (SignalDynamic _ _ vOld) (SignalDynamic _ _ vNew) (ConnectionVariableIR variableIR Nothing)  =
    case (testBit vOld 0, testBit vNew 0 ) of
        (False,True) -> True
        _ -> False

isConnectionSignalChangeTriggerNegedge (SignalDynamic _ _ vOld) (SignalDynamic _ _ vNew) (ConnectionVariableIR variableIR Nothing) =
    case (testBit vOld 0, testBit vNew 0) of
        (True,False) -> True
        _ -> False

doesSignalChangeTriggerEvent ::SignalDynamic -> SignalDynamic -> EventExpressionIR -> Bool
doesSignalChangeTriggerEvent sigOld sigNew eventExpression =
    let edgeDetectionMethod = case eventExpression.edgeIdentifier of
                                    Just Posedge -> isConnectionSignalChangeTriggerPosedge
                                    Just Negedge -> isConnectionSignalChangeTriggerNegedge
                                    Just Edge -> isConnectionSignalChangeTriggerLevel
                                    Nothing ->  isConnectionSignalChangeTriggerLevel
        matchedSignal = sigOld.signalVariableOrNetIdentifier == fetchNetVariableIdentifierFromConnection eventExpression.connection
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
        case Map.lookup variableOrNetIdentifier sensitiveProcessMap of
            Nothing -> return ()
            Just sensitiveProcesses ->
                Set.foldl (\s elem ->
                    if doesSignalChangeTriggerProcess oldSig newSig elem then
                        s >> updateTimeSlotFromSensitiveProcess elem
                    else s) (pure ()) sensitiveProcesses

generateTimeSlotFromTestbench ::
    IR
    -> Map.Map VariableOrNetIdentifierIR (ReifiedLens' circuitState SignalDynamic)
    -> TestbenchCircuitState circuitState
    -> TimeSlot
generateTimeSlotFromTestbench ir lensMap testbenchCircuitState =
    let sensitiveProcessMap = mkSensitiveProcessesMapFromIR ir
        timeSlotUpdater = generateTimeSlotFromTestbenchSignalUpdate sensitiveProcessMap lensMap (view oldState testbenchCircuitState) (view updatedState testbenchCircuitState)
        timeSlotGenerator = foldl (\ts elem -> ts >> timeSlotUpdater elem) (pure ()) (view updatedSignals testbenchCircuitState)
    in execState timeSlotGenerator emptyTimeSlot

executeEvent :: IR
                -> Map.Map VariableOrNetIdentifierIR (ReifiedLens' circuitState SignalDynamic)
                -> TimeSlotEvent
                -> State (circuitState, TimeSlot) ()
executeEvent ir lensMap (UpdateNetVariable toSet (EConnection toFetch)) = do
    let fetchLens = lensMap Map.! fetchNetVariableIdentifierFromConnection toFetch
    let setLens = lensMap Map.! fetchNetVariableIdentifierFromConnection toSet
    (oldState, oldTimeSlot) <- get
    let fetchedValue = view (runLens fetchLens) oldState
    let updatedState = set (runLens setLens) fetchedValue oldState
    let sensitiveProcessMap = mkSensitiveProcessesMapFromIR ir
    let timeSlotUpdater = generateTimeSlotFromTestbenchSignalUpdate sensitiveProcessMap lensMap updatedState oldState (fetchNetVariableIdentifierFromConnection toFetch)
    let newTimeSlot = execState timeSlotUpdater oldTimeSlot
    put (updatedState, newTimeSlot)

executeEvent ir lensMap (UpdateNetVariable toSet (ELiteral lit)) = do
    (oldState, oldTimeSlot) <- get
    let ln = lensMap Map.! fetchNetVariableIdentifierFromConnection toSet
    let newValue = (view (runLens ln) oldState) {signalValue = lit}
    let updatedState = over (runLens ln) (const newValue) oldState
    put (updatedState, oldTimeSlot)

executeEvent ir lensMap (ScheduleNBAUpdate conn (EConnection toFetch)) = do
    let fetchLens = lensMap Map.! fetchNetVariableIdentifierFromConnection toFetch
    (oldState, oldTimeSlot) <- get
    let fetchedValue = view (runLens fetchLens) oldState
    let newTimeSlot = addToNbaRegion oldTimeSlot $ UpdateNetVariable  conn $ ELiteral fetchedValue.signalValue
    put (oldState, newTimeSlot)

executeEvents ::
    IR
    -> Map.Map VariableOrNetIdentifierIR (ReifiedLens' circuitState SignalDynamic)
    -> Queue.Queue TimeSlotEvent
    -> State (circuitState, TimeSlot) ()
executeEvents ir lensMap q =
    traceShow q
    foldl (\state event -> state >> executeEvent ir lensMap event) (pure () ) q

executeActiveRegionSet :: IR -> Map.Map VariableOrNetIdentifierIR (ReifiedLens' circuitState SignalDynamic) -> State (circuitState, TimeSlot) ()
executeActiveRegionSet ir lensMap= do
    events <- getFirstRegionInActiveRegionSet
    case events of
        Nothing -> return ()
        Just e -> do
                    traceShow "events" $ pure ()
                    executeEvents ir lensMap e
                    executeActiveRegionSet ir lensMap

executeReActiveRegionSet ::
    IR
    -> Map.Map VariableOrNetIdentifierIR (ReifiedLens' circuitState SignalDynamic)
    -> State (circuitState, TimeSlot) ()
executeReActiveRegionSet ir lensMap = do
    events <- getFirstRegionInReactiveRegionSet
    traceShow events $ pure ()
    case events of
        Just e -> do
                    executeEvents ir lensMap e
                    executeReActiveRegionSet ir lensMap
        Nothing -> return ()

eval :: (Show circuitState) =>  IR -> Map.Map VariableOrNetIdentifierIR (ReifiedLens' circuitState SignalDynamic) -> State (TestbenchCircuitState circuitState) ()
eval ir lensMap = do
    testbenchCircuitState <- get
    let timeSlot = generateTimeSlotFromTestbench ir lensMap testbenchCircuitState
    let newState = fst $ execState (executeTimeSlot ir lensMap) (view updatedState testbenchCircuitState, generateTimeSlotFromTestbench ir lensMap testbenchCircuitState)
    put $ TestbenchCircuitState newState newState Set.empty