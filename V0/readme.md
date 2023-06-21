# V2H Project

## Where to find interesting files?

### Phase 1 Files
* Lexer built using Alex -
[src/V2H/Alex/Lexer.x](src/V2H/Alex/Lexer.x)
* Parser built using Earley Library - [src/V2H/Parser.hs](src/V2H/Alex/Lexer.x)
* Stage 1 and Stage2 IR Generation - [src/V2H/IRGenerator.hs](src/V2H/IRGenerator.hs)
* Code Generation Functions - [src/V2H/CodeGenerator.hs](src/V2H/CodeGenerator.hs)

### Files for EDSL
* EDSL utility functions - [src/V2H/Simulator/EDSL.HS](src/V2H/Simulator/EDSL.HS)

### Simulator files

* Entry point for simulator - [src/V2H/Simulator/Simulate.hs](src/V2H/Simulator/Simulate.hs)
* Modelling System Verilog data types within the Simulator - [src/V2H/Simulators/Simulate.hs](src/V2H/Simulators/Simulate.hs)
* Files for handling System Verilog Timeslot - [src/V2H/Simulator/TimeSlot.hs](src/V2H/Simulator/TimeSlot.hs)

### Unit Tests

* Ripple Counter Test - [tests/unit_tests/ripple_counter/testbench.hs](tests/unit_tests/ripple_counter/testbench.hs)

* Parallel In Serial Out Shift Register - [tests/unit_tests/piso/testbench.hs](tests/unit_tests/piso/testbench.hs)

* Serial In Parallel Out Shift Register - [tests/unit_tests/sipo/testbench.hs](tests/unit_tests/sipo/testbench.hs)

* Fipper Module - [tests/unit_tests/flipper/testbench.hs](tests/unit_tests/flipper/testbench.hs)
