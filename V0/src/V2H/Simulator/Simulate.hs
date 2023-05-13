module V2H.Simulator.Simulate where
import Control.Monad
import Control.Monad.State.Strict
import Data.Function
import Data.Sequence.Queue qualified as Queue


import V2H.IR

data Event = UpdateSignal ConnectionIR Int deriving (Show, Eq)

data TimeSlot =
    TimeSlot {
        activeRegion :: Queue.Queue Event,
        nbaRegion :: Queue.Queue Event,
        reactiveRegion :: Queue.Queue Event,
        reNbaRegion :: Queue.Queue Event
    } deriving (Show)


updateSignal :: IR -> topLevel -> circuitState ->TimeSlot
updateSignal ir topLevel circuitState =


executeRegion :: IR -> State (circuitState, TimeSlot) ()
executeRegion =
    undefined

-- execute
-- executeActiveRegion = undefined
-- execute

getFirstRegionInActiveRegionSet :: State (circuitState, TimeSlot) (Maybe (Queue.Queue Event))
getFirstRegionInActiveRegionSet = do
    (t, ts) <- get
    let emptyActiveRegion = do
            put (t, ts {activeRegion = Queue.empty})
            return $ Just ts.activeRegion
    let emptyNbaRegion = do
            put (t, ts {nbaRegion = Queue.empty})
            return $ Just  ts.nbaRegion

    if ts.activeRegion /= Queue.empty then emptyActiveRegion
    else if ts.activeRegion /= Queue.empty then emptyNbaRegion
    else return Nothing


executeEvent :: IR -> Event -> State (circuitState, TimeSlot) ()
executeEvent =
    undefined

executeEvents :: IR  -> Queue.Queue Event -> State (circuitState, TimeSlot) ()
executeEvents ir =
    foldl (\state event -> state >> executeEvent ir event) $ pure ()

executeActiveRegionSet :: IR -> State (circuitState, TimeSlot) ()
executeActiveRegionSet ir = do
    events <- getFirstRegionInActiveRegionSet
    case events of
        Just e -> do
                    executeEvents ir e
                    executeActiveRegionSet ir
        Nothing -> return ()

executeReactiveRegionSet :: IR -> State (circuitState, TimeSlot) Bool
executeReactiveRegionSet ir = undefined

isTimeSlotEmpty :: State (circuitState,TimeSlot) Bool
isTimeSlotEmpty = do
    (_, ts) <- get
    return (ts.activeRegion == Queue.empty
            && ts.nbaRegion == Queue.empty
            && ts.reactiveRegion == Queue.empty
            && ts.reNbaRegion == Queue.empty)

executeTimeSlot :: IR -> State (circuitState,TimeSlot) ()
executeTimeSlot ir = do
    executeActiveRegionSet ir
    executeReactiveRegionSet ir
    remainingEvents <- isTimeSlotEmpty
    when remainingEvents $ executeTimeSlot ir

eval :: IR -> toplevel -> State circuitState ()
eval ir inputChanges = do
    t <- get
    put $ fst $ execState (executeTimeSlot ir) (t, updateSignal ir inputChanges)
