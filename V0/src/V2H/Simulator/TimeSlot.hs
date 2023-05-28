{-# LANGUAGE TemplateHaskell#-}
module V2H.Simulator.TimeSlot where

import Data.Sequence.Queue qualified as Queue
import Control.Lens
import V2H.IR
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


addToActiveRegion :: TimeSlot -> TimeSlotEvent -> TimeSlot
addToActiveRegion = addToRegion activeRegion
addToNbaRegion :: TimeSlot -> TimeSlotEvent -> TimeSlot
addToNbaRegion = addToRegion nbaRegion
addToReactiveRegion :: TimeSlot -> TimeSlotEvent -> TimeSlot
addToReactiveRegion = addToRegion reActiveRegion
addToReNbaRegion :: TimeSlot -> TimeSlotEvent -> TimeSlot
addToReNbaRegion = addToRegion reNbaRegion

addToRegion regionLens timeSlot event = over regionLens (Queue.|> event) timeSlot
