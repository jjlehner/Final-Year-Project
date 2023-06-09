{-# LANGUAGE TemplateHaskell #-}
module V2H.Simulator.TimeSlot where

import Data.Sequence.Queue qualified as Queue
import Control.Lens
import Control.Monad.State.Strict
import V2H.IR
import V2H.Simulator.Circuit
data TimeSlotEvent = UpdateNetVariable ConnectionIR ExpressionIR | ScheduleNBAUpdate ConnectionIR ExpressionIR deriving (Show, Eq)

data TimeSlot =
    TimeSlot {
        _activeRegion :: Queue.Queue TimeSlotEvent,
        _nbaRegion :: Queue.Queue TimeSlotEvent,
        _reActiveRegion :: Queue.Queue TimeSlotEvent,
        _reNbaRegion :: Queue.Queue TimeSlotEvent
    } deriving (Show)

$(makeLenses ''TimeSlot)

emptyTimeSlot = TimeSlot Queue.empty Queue.empty Queue.empty Queue.empty

popFirstRegionInActiveRegionSet :: State (circuitState, TimeSlot) (Maybe (Queue.Queue TimeSlotEvent))
popFirstRegionInActiveRegionSet = do
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

popFirstRegionInReactiveRegionSet :: State (circuitState, TimeSlot) (Maybe (Queue.Queue TimeSlotEvent))
popFirstRegionInReactiveRegionSet = do
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

isTimeSlotEmpty :: State (DynamicCircuitState,TimeSlot) Bool
isTimeSlotEmpty = do
    (_, ts) <- get
    return (ts._activeRegion == Queue.empty
            && ts._nbaRegion == Queue.empty
            && ts._reActiveRegion == Queue.empty
            && ts._reNbaRegion == Queue.empty)

addToActiveRegion :: TimeSlot -> TimeSlotEvent -> TimeSlot
addToActiveRegion = addToRegion activeRegion
addToNbaRegion :: TimeSlot -> TimeSlotEvent -> TimeSlot
addToNbaRegion = addToRegion nbaRegion
addToReactiveRegion :: TimeSlot -> TimeSlotEvent -> TimeSlot
addToReactiveRegion = addToRegion reActiveRegion
addToReNbaRegion :: TimeSlot -> TimeSlotEvent -> TimeSlot
addToReNbaRegion = addToRegion reNbaRegion

addToRegion regionLens timeSlot event = over regionLens (Queue.|> event) timeSlot
