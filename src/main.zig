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
    _ = @import("decode.zig");
    _ = @import("simulate.zig");
    _ = @import("names.zig");
}

const std = @import("std");
const dec = @import("decode.zig");
const sim = @import("simulate.zig");
