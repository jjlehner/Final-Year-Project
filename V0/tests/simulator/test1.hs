module Main where
import V2H.Simulator.Simulate
import Data.Map qualified as Map
import V2H.IR
import Control.Monad.State.Strict

data ExampleModule = ExampleModule {
    clk :: Int,
    a :: Int,
    b :: Int
} deriving (Show)

clkIRIdentifier = ModuleItemIdentifierIR "clk"
aIRIdentifier = ModuleItemIdentifierIR "a"
bIRIdentifier = ModuleItemIdentifierIR "b"

clkIR = VariableIR clkIRIdentifier LogicIR Nothing Nothing
aIR = VariableIR aIRIdentifier LogicIR Nothing Nothing
bIR = VariableIR  bIRIdentifier LogicIR Nothing Nothing

clkIRConnection = ConnectionVariableIR clkIR Nothing
aIRConnection = ConnectionVariableIR aIR Nothing
bIRConnection = ConnectionVariableIR bIR Nothing

sItems = [NonblockingAssignment bIRConnection $ EConnection aIRConnection]
ir = IR {
    alwaysConstructs = Map.fromList [(  ModuleItemIdentifierIR "ExampleModuleAlways1",
                                        AlwaysConstructIR {
                                            identifier                              = ModuleItemIdentifierIR "ExampleModuleAlways1",
                                            sensitivity                             = FF [EventExpressionIR {connection=clkIRConnection, edgeIdentifier=Posedge}],
                                            inputConnections                        = [clkIRConnection],
                                            outputConnections                       = [aIRConnection],
                                            statementItems                          = sItems
                                        })],
    variables = Map.fromList $ [    (clkIRIdentifier, clkIR),
                                    (aIRIdentifier, aIR),
                                    (bIRIdentifier, bIR)    ],
    nets = Map.empty
}
main :: IO ()
main =
    let start = ExampleModule {
                    clk = 0,
                    a = 1,
                    b = 0
                }
        update = start { clk = 1 }
        outputState = execState (eval ir update) start
    in  print outputState


{-
    module ExampleModule (
        input    logic       clk,
        input    logic       a,
        output   logic       b
    );
        always_ff@(posedge clk) begin
            b <= a;
        end
    endmodule -}