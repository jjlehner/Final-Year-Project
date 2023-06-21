module flipper (
    input logic clk,
    output logic d
);
    always_ff @(posedge clk) begin
        d <= !d;
    end

endmodule