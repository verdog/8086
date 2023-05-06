//! Test helpers and utilities

fn compileAsmFile(
    comptime basename: []const u8,
    comptime input_dir: []const u8,
    comptime output_postfix: []const u8,
    alctr: std.mem.Allocator,
) !void {
    const res = try std.ChildProcess.exec(.{
        .allocator = alctr,
        .argv = &.{
            "nasm",
            "./" ++ input_dir ++ "/" ++ basename ++ ".asm",
            "-o",
            "./testfs/" ++ basename ++ output_postfix,
        },
    });
    defer alctr.free(res.stdout);
    defer alctr.free(res.stderr);

    errdefer std.debug.print("stdout:\n{s}\n", .{res.stdout});
    errdefer std.debug.print("stderr:\n{s}\n", .{res.stderr});
    try expectEq(@as(u8, res.term.Exited), 0);
}

/// Run an end to end decoder test. Given a "basename" that represents a file,
/// 1. Compile ./asm/<basename>.asm to a ./testfs/<basename> binary using nasm
/// 2. Decode the ./asm/<basename> binary with our decoder and write the decoded assembly
///    to ./testfs/<basename>.asm
/// 3. Compile the ./testfs/<basename>.asm to a ./testfs/<basename>2 binary using nasm
/// 4. Assert that the ./testfs/<basename> and ./testfs/<basename>2 binaries are identical
pub fn decodeEndToEnd(comptime basename: []const u8, alctr: std.mem.Allocator) !void {
    // compile reference asm
    try compileAsmFile(basename, "asm", "", alctr);

    // decode binary
    {
        const decoded = try std.fs.cwd().createFile("./testfs/" ++ basename ++ ".asm", .{});
        defer decoded.close();
        try dec.decodeAndPrintFile("./testfs/" ++ basename, decoded.writer(), alctr);
    }

    // compile decoded asm
    try compileAsmFile(basename, "testfs", "2", alctr);

    // ensure they are identical
    {
        const ref = try std.fs.cwd().openFile("./testfs/" ++ basename, .{});
        defer ref.close();
        const ours = try std.fs.cwd().openFile("./testfs/" ++ basename ++ "2", .{});
        defer ours.close();

        const ref_data = try ref.readToEndAlloc(alctr, 1024 * 1024);
        defer alctr.free(ref_data);
        const ours_data = try ours.readToEndAlloc(alctr, 1024 * 1024);
        defer alctr.free(ours_data);

        try std.testing.expectEqualSlices(u8, ref_data, ours_data);
    }
}

pub fn simulateEndToEnd(comptime basename: []const u8, alctr: std.mem.Allocator) !void {
    // generate binary file to simulate
    try compileAsmFile(basename, "asm", "", alctr);

    // simulate it
    const simulated = try std.fs.cwd().createFile("./testfs/" ++ basename ++ ".sim", .{});
    defer simulated.close();
    try sim.simulateAndPrintFile("./testfs/" ++ basename, simulated.writer(), alctr);
}

const std = @import("std");
const expectEq = std.testing.expectEqual;

const dec = @import("decode.zig");
const sim = @import("simulate.zig");
