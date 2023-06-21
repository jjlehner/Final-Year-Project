{-# LANGUAGE TemplateHaskell, DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module V2H.Simulator.Signal where
import GHC.TypeLits
import Control.Lens.TH
import V2H.IR qualified as IR
import V2H.IR.DataTypes qualified as IR
import V2H.IRGenerator.Expressions qualified as IR
import Data.Kind
import Data.Type.Bool
import Data.Data
data SignalChange (sig) a = SignalChange {_start::a, _end::a} deriving (Show,Eq)
mkStable a = SignalChange a a

mkStableFromSignalValue a = mkStable $ Signal $ IR.signalValueToInteger a

type CheckBitSize :: Nat -> Nat -> Constraint
type family CheckBitSize (n :: Nat) (a :: Nat) :: Constraint where
  CheckBitSize n a = If (a <=? (2 ^ n - 1)) (() :: Constraint) (TypeError (Text "Literal too large"))


mkSignal :: forall a n  . (CheckBitSize n a, KnownNat a) => Signal n
mkSignal = Signal $ natVal $ Proxy @a

newtype Signal (a::Nat) = Signal {value::Integer} deriving (Show, Eq)
$(makeLenses ''SignalChange)

