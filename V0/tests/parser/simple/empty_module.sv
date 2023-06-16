module top (
    input logic clk,
    input logic dTop,
    output logic z,
    output logic c
);

    submodule w (
        .clk1(clk),
        .cl(dTop),
        .changingSignal(z)
    );

    submodule x (
        .clk1(clk),
        .cl(dTop),
        .changingSignal(c)
    );

    submodule x1 (
        .clk1(clk),
        .cl(dTop),
        .changingSignal(c)
    );
    submodule x2 (
        .clk1(clk),
        .cl(dTop),
        .changingSignal(c)
    );
    submodule x3 (
        .clk1(clk),
        .cl(dTop),
        .changingSignal(c)
    );
    submodule x4 (
        .clk1(clk),
        .cl(dTop),
        .changingSignal(c)
    );
    submodule x5 (
        .clk1(clk),
        .cl(dTop),
        .changingSignal(c)
    );
    submodule x6 (
        .clk1(clk),
        .cl(dTop),
        .changingSignal(c)
    );
    submodule x7 (
        .clk1(clk),
        .cl(dTop),
        .changingSignal(c)
    );

    submodule x8 (
        .clk1(clk),
        .cl(dTop),
        .changingSignal(c)
    );
    submodule x9 (
        .clk1(clk),
        .cl(dTop),
        .changingSignal(c)
    );
    submodule x10 (
        .clk1(clk),
        .cl(dTop),
        .changingSignal(c)
    );
    submodule x11 (
        .clk1(clk),
        .cl(dTop),
        .changingSignal(c)
    );
    submodule x12 (
        .clk1(clk),
        .cl(dTop),
        .changingSignal(c)
    );
    submodule x13 (
        .clk1(clk),
        .cl(dTop),
        .changingSignal(c)
    );
    submodule x14 (
        .clk1(clk),
        .cl(dTop),
        .changingSignal(c)
    );
endmodule