module piso (
    input logic clk,
    input logic uptake,
    input logic enable,
    input logic in0,
    input logic in1,
    input logic in2,
    input logic in3,
    input logic in4,
    input logic in5,
    output logic out
);
    logic a0;
    logic a1;
    logic a2;
    logic a3;
    logic a4;
    logic a5;
    logic a6;

    always_comb begin
        a0 = 0;
        out = a6;
    end

    stager s0(
        .clk(clk),
        .prev(a0),
        .in(in0),
        .next(a1),
        .uptake(uptake),
        .enable(enable)
    );

    stager s1(
        .clk(clk),
        .prev(a1),
        .in(in1),
        .next(a2),
        .uptake(uptake),
        .enable(enable)
    );
    stager s2(
        .clk(clk),
        .prev(a2),
        .in(in2),
        .next(a3),
        .uptake(uptake),
        .enable(enable)
    );
    stager s3(
        .clk(clk),
        .prev(a3),
        .in(in3),
        .next(a4),
        .uptake(uptake),
        .enable(enable)
    );
    stager s4(
        .clk(clk),
        .prev(a4),
        .in(in4),
        .next(a5),
        .uptake(uptake),
        .enable(enable)
    );
    stager s5(
        .clk(clk),
        .prev(a5),
        .in(in5),
        .next(a6),
        .uptake(uptake),
        .enable(enable)
    );


endmodule