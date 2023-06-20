module V2H.Simulator.EDSL where
import Data.Set qualified as Set
import V2H.IR
import V2H.Simulator.Circuit
import V2H.Simulator.Simulate
import Control.Monad.State.Strict
import Control.Lens
import V2H.Simulator.Signal
(<==) :: (FetchHierarchicalIdentifierIR (SignalChange a b))
    => Lens' t (SignalChange a b)
    -> b
    -> State (StimulatedCircuit t) ()
(<==) ln newValue = do
    (StimulatedCircuit circuitState changedSignals) <- get
    let b = set (ln . end) newValue circuitState
    put $ StimulatedCircuit b $ Set.insert (fetchHierarchicalIdentifierIR $ view ln circuitState) changedSignals

fetchValue ::
    Lens' c (SignalChange a (Signal v))
    -> State (StimulatedCircuit c) (Signal v)
fetchValue ln = do
    stim <- get
    let value = view ln $ _stimulatedState stim
    pure $ _start value

evalStimulatedCircuit ::
    ExpandedIR
    -> (StimulatedCircuit a -> StimulatedCircuit (DynamicCircuitState, DynamicCircuitState))
    -> (DynamicCircuitState -> a)
    ->  State (StimulatedCircuit a) ()
evalStimulatedCircuit expandedIR convertToDynamic convertFromDynamic = do
    stimulatedCircuit <- get
    let stimulatedCircuitDynamic = convertToDynamic stimulatedCircuit
    let newStateDynamic =  eval expandedIR stimulatedCircuitDynamic
    let newState = convertFromDynamic newStateDynamic
    put $ StimulatedCircuit newState Set.empty
