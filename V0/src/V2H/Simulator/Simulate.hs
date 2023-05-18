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
        variableOrNetIdentifier :: VariableOrNetIdentifierIR,
        width :: Integer,
        value :: Integer
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

executeEvent :: IR -> TimeSlotEvent -> State (circuitState, TimeSlot) ()
executeEvent =
    undefined

executeEvents :: IR  -> Queue.Queue TimeSlotEvent -> State (circuitState, TimeSlot) ()
executeEvents ir q =
    traceShow q
    foldl (\state event -> state >> executeEvent ir event) (pure () ) q

executeActiveRegionSet :: IR -> State (circuitState, TimeSlot) ()
executeActiveRegionSet ir = do
    events <- getFirstRegionInActiveRegionSet
    case events of
        Nothing -> return ()
        Just e -> do
                    traceShow "events" $ pure ()
                    executeEvents ir e
                    executeActiveRegionSet ir

executeReActiveRegionSet :: IR -> State (circuitState, TimeSlot) ()
executeReActiveRegionSet ir = do
    events <- getFirstRegionInReactiveRegionSet
    traceShow events $ pure ()
    case events of
        Just e -> do
                    executeEvents ir e
                    executeReActiveRegionSet ir
        Nothing -> return ()

isTimeSlotEmpty :: State (circuitState,TimeSlot) Bool
isTimeSlotEmpty = do
    (_, ts) <- get
    return (ts._activeRegion == Queue.empty
            && ts._nbaRegion == Queue.empty
            && ts._reActiveRegion == Queue.empty
            && ts._reNbaRegion == Queue.empty)

data TestbenchCircuitState a = TestbenchCircuitState a a (Set.Set VariableOrNetIdentifierIR) deriving (Show)

executeTimeSlot :: IR -> State (circuitState,TimeSlot) ()
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
        let addToReactiveRegion timeSlotUpdated event = over reActiveRegion (Queue.|> statementItemToTimeSlotEvent event) timeSlotUpdated
        put $ foldl addToReactiveRegion timeSlotInit alwaysConstruct.statementItems
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
        matchedSignal = sigOld.variableOrNetIdentifier == fetchNetVariableIdentifierFromConnection eventExpression.connection
    in matchedSignal || edgeDetectionMethod sigOld sigNew eventExpression.connection

doesSignalChangeTriggerProcess :: SignalDynamic -> SignalDynamic -> AlwaysConstructIR -> Bool
doesSignalChangeTriggerProcess sigOld sigNew alwaysConstruct =
    case alwaysConstruct.sensitivity of
        Comb -> any (isConnectionSignalChangeTriggerLevel sigOld sigNew) alwaysConstruct.inputConnections
        FF eventExpressions -> any (doesSignalChangeTriggerEvent sigOld sigNew) eventExpressions
        Latch -> any (isConnectionSignalChangeTriggerLevel sigOld sigNew) alwaysConstruct.inputConnections

generateTimeSlotFromTestbenchSignalUpdate ::
    Map.Map VariableOrNetIdentifierIR (Set.Set AlwaysConstructIR)
    -> Map.Map VariableOrNetIdentifierIR (Getting SignalDynamic a SignalDynamic)
    -> a
    -> a
    -> VariableOrNetIdentifierIR
    -> State TimeSlot ()
generateTimeSlotFromTestbenchSignalUpdate sensitiveProcessMap lensMap oldState testbenchState variableOrNetIdentifier =
    do
        timeslot <- get
        let lens = lensMap Map.! variableOrNetIdentifier
        let oldSig = view lens oldState
        let newSig = view lens testbenchState
        case Map.lookup variableOrNetIdentifier sensitiveProcessMap of
            Nothing -> return ()
            Just sensitiveProcesses ->
                Set.foldl (\s elem ->
                    if doesSignalChangeTriggerProcess oldSig newSig elem then
                        s >> updateTimeSlotFromSensitiveProcess elem
                    else s) (pure ()) sensitiveProcesses

generateTimeSlotFromTestbench ::
    IR
    -> Map.Map VariableOrNetIdentifierIR (Getting SignalDynamic a SignalDynamic)
    -> TestbenchCircuitState a
    -> TimeSlot
generateTimeSlotFromTestbench ir lensMap (TestbenchCircuitState circuitState testbenchState changingSignals) =
    let sensitiveProcessMap = mkSensitiveProcessesMapFromIR ir
        timeSlotUpdater = generateTimeSlotFromTestbenchSignalUpdate sensitiveProcessMap lensMap circuitState testbenchState
        timeSlotGenerator = foldl (\ts elem -> ts >> timeSlotUpdater elem) (pure ()) changingSignals
    in execState timeSlotGenerator emptyTimeSlot

eval :: (Show a) =>  IR -> Map.Map VariableOrNetIdentifierIR (Getting SignalDynamic a SignalDynamic) -> State (TestbenchCircuitState a) ()
eval ir lensMap = do
    testbenchCircuitState@(TestbenchCircuitState testbenchState oldState _) <- get
    let timeSlot = generateTimeSlotFromTestbench ir lensMap testbenchCircuitState
    let newState = fst $ execState (executeTimeSlot ir) (testbenchState, generateTimeSlotFromTestbench ir lensMap testbenchCircuitState)
    put $ TestbenchCircuitState newState newState Set.empty