module top(
    input logic clk,
    output logic a,
    output logic b,
    output logic c,
    output logic d,
    output logic e
);
    logic aprime;
    logic bprime;
    logic cprime;
    logic dprime;
    logic eprime;


    always_comb begin
        a = !aprime;
        b = !bprime;
        c = !cprime;
        d = !dprime;
        e = !eprime;
    end

    ripple ra(
        .clk(clk),
        .out(aprime)
    );

    ripple rb(
        .clk(aprime),
        .out(bprime)
    );

    ripple rc(
        .clk(bprime),
        .out(cprime)
    );

    ripple rd(
        .clk(cprime),
        .out(dprime)
    );

    ripple re(
        .clk(dprime),
        .out(eprime)
    );

endmodule
