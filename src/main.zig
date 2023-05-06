pub fn main() !void {
    if (std.os.argv.len < 2) {
        std.debug.print("Usage: ./exe <filename>\n", .{});
        return;
    }

    var gpa_impl = std.heap.GeneralPurposeAllocator(.{}){};
    const gpa = gpa_impl.allocator();
    defer if (!gpa_impl.detectLeaks()) std.debug.print("(No leaks)\n", .{});

    const filename = std.mem.span(std.os.argv[1]);
    try dec.decodeAndPrintFile(filename, std.io.getStdOut().writer(), gpa);

    try sim.simulateAndPrintFile(filename, std.io.getStdOut().writer(), gpa);
    return;
}

test {
    // it makes sense to run decode tests before simulate tests, since the simulator
    // depends on correct results from the decoder.
    _ = @import("decode.zig");
    _ = @import("simulate.zig");
    _ = @import("names.zig");
    _ = @import("text.zig");
    _ = @import("test.zig");
}

const std = @import("std");
const dec = @import("decode.zig");
const sim = @import("simulate.zig");
