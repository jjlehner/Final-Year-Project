import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

df = pd.read_csv("tests/unit_tests/piso/plot.csv")
# plt.scatter(df["simulation_cycle_length"],df["average_cycles_simulated_per_second"],marker='x')
plt.xscale("log")
plt.errorbar(
    df["simulation_cycle_length"],
    df["average_cycles_simulated_per_second"],
    yerr=[df["max_err"],df["min_err"]],
    fmt = "x",
    markersize=10,
    capsize=10,
    markeredgecolor='r'
    )
plt.xlabel("Simulation Cycle Length /(clock-cycles-simulated)", size=15,fontweight="bold")
plt.ylabel("Average Rate of Simulation\n /(clock-cycles-simulated / second)", size=15,fontweight="bold")
plt.title("Verilator Simulation\nParallel In Serial Out Shift Register Simulation\nAverage Rate of Simulation (clock-cycles-simulated / second)\nvs Simulation Cycle Length /(clock-cycles)", size=15,fontweight="bold")
plt.grid()
plt.show()


# --------------

df = pd.read_csv("tests/unit_tests/piso/V2H_plot.csv")
plt.scatter(df["simulation_cycle_length"],df["simulation_rate_int"],marker='x',label="Simulation rate where signals are Int values")
plt.scatter(df["simulation_cycle_length"],df["simulation_rate_strict"],marker='x', label="Simulation rate where strict monad is used ")
plt.scatter(df["simulation_cycle_length"],df["simulation_rate_lazy"],marker='x', label="Simulation rate where lazy monad is used")
plt.legend()
plt.xscale("log")
plt.xlabel("Simulation Cycle Length /(clock-cycles-simulated)", size=15,fontweight="bold")
plt.ylabel("Average Rate of Simulation\n /(clock-cycles-simulated / second)", size=15,fontweight="bold")
plt.title("V2H Simulator\nParallel In Serial Out Shift Register Simulation\nAverage Rate of Simulation (clock-cycles-simulated / second)\nvs Simulation Cycle Length /(clock-cycles)", size=15,fontweight="bold")
plt.yticks(np.arange(900,1500,25), minor=True)
plt.grid(which="both")

plt.show()
