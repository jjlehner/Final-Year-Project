module ripple(
    input logic clk,
    output logic out
);

always_ff@(posedge clk) begin
    out <= !out;
end


endmodule