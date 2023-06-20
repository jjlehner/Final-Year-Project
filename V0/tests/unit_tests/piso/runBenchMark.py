import subprocess
import sys

i = 1
f = open("tests/unit_tests/piso/times.txt", "w")
while i < 100000000:
    i *= 2
    executable = "dist-newstyle/build/aarch64-osx/ghc-9.2.7/Verilog2Haskell-0.1.0.0/t/piso-test/build/piso-test/piso-test"
    subprocess.run(["time", executable, str(i)], stderr=f)

