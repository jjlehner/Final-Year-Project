{-# LANGUAGE TemplateHaskell, DataKinds #-}
module V2H.Simulator.Signal where
import GHC.TypeLits
import Control.Lens.TH
import V2H.IR qualified as IR
import V2H.IR.DataTypes qualified as IR
import V2H.Simple.IRGenerator.Expressions qualified as IR
data SignalChange (sig) a = SignalChange {_start::a, _end::a} deriving (Show,Eq)
mkStable a = SignalChange a a

-- Broken should take datatype as a parameter assumes everything is one bit
mkStableFromSignalValue a = mkStable $ Signal $ IR.signalValueToInteger a (IR.DTSingular $ IR.STScalar IR.SIVTLogic)
newtype Signal (a::Nat) = Signal {value::Integer} deriving (Show, Eq)
$(makeLenses ''SignalChange)

signalChangeToDynamic signalChange = IR.SignalValue (IR.DTSingular $ IR.STScalar IR.SIVTLogic) $ IR.mkSignalValueDataObjectFromInteger signalChange._end.value