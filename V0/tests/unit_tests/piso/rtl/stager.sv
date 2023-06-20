module stager(
    input logic clk,
    input logic prev,
    input logic in,
    output logic next,
    input logic uptake,
    input logic enable
);

    logic nextprime;
    logic [2:0] z;
    always_comb begin
        if(enable) begin
            if(uptake) begin
                z = 1;
                nextprime = in;
            end else begin
            z = 2;
                nextprime = prev;
            end
        end else begin
            z = 3;
            nextprime = next;
        end
    end

    always_ff@(posedge clk) begin
        next <= nextprime;
    end

endmodule