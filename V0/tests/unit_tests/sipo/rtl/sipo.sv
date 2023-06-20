module sipo(
    input logic clk,
    input logic enable,
    input logic serial_in,
    output logic [3:0] parallel_out
);

logic d1;
logic d2;
logic d3;
logic d4;


always_comb begin
    parallel_out = {d1,d2,d3,d4};
end

stager s0(
    .clk(clk),
    .enable(enable),
    .datain(serial_in),
    .dataout(d1)
);

stager s1(
    .clk(clk),
    .enable(enable),
    .datain(d1),
    .dataout(d2)
);

stager s2(
    .clk(clk),
    .enable(enable),
    .datain(d2),
    .dataout(d3)
);

stager s3(
    .clk(clk),
    .enable(enable),
    .datain(d3),
    .dataout(d4)
);

endmodule