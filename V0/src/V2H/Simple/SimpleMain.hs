{-# LANGUAGE TemplateHaskell, DataKinds #-}
module Main where
import V2H.Simulator.Signal
import V2H.Simple.V2H

$(setup "top" ["tests/parser/simple/empty_module.sv", "tests/parser/simple/submodule.sv"])

-- x = Top {
--     _a = Signal 0,
--     _b = Signal 0,
--     _clk = Signal 0,
--     _sub=
--         Sub{_a = Signal 0, _clk=Signal 0}, _sub2=Sub2{_a = Signal 0, _clk=Signal 0}}
main = undefined