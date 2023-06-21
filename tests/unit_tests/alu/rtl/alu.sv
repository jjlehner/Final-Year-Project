module alu (
    input logic [1:0] a,
    input logic [1:0] b,
    input logic clk,
    input logic [3:0] operation,
    output logic [3:0] out
);
    logic [3:0] out_next;
    always_comb begin
        if(operation == 2) begin
            out_next = a * b;
        end else if (operation == 3) begin
            out_next = a + b;
        end else begin
            out_next = a - b + 1;
        end
    end

    always_ff @(posedge clk) begin
        out <= out_next;
    end

endmodule