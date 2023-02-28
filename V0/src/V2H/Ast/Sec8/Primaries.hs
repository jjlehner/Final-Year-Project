module V2H.Ast.Sec8.Primaries where
import V2H.Ast.Sec8.Numbers
-- | Incomplete production rule
data ConstantSelect = ConstantSelect deriving (Show)

data TimeLiteral =  UnsignedTL UnsignedNumber TimeUnit
                    | FixedPointTL UnsignedNumber UnsignedNumber Timeunit

data TimeUnit = Second | Millisecond | Microsecond | Nanosecond | Picosecond | Femtosecond
