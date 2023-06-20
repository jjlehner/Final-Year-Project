module stager (
    input logic clk,
    input logic enable,
    input logic datain,
    output logic dataout
);

logic dataoutnext;
always_comb begin
    if(enable)
        dataoutnext = datain;
end

always_ff@(posedge clk) begin
    dataout <= dataoutnext;
end

endmodule
