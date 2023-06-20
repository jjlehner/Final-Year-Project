import subprocess
import sys

i = 1
f = open("tests/unit_tests/piso/verilator/times-verilator.txt", "w")
while i < 10000000000000:
    i *= 2
    executable = "tests/unit_tests/piso/verilator/obj_dir/Vpiso"
    subprocess.run(["time", executable, str(i)], stderr=f)

