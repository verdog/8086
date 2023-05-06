//! CPU/Memory simulation

const RegisterSlots = union {
    x: u16,
    n: union {
        h: u8,
        l: u8,
    },
};

const Registers = struct {
    a: RegisterSlots = RegisterSlots{ .x = 0 },
    c: RegisterSlots = RegisterSlots{ .x = 0 },
    d: RegisterSlots = RegisterSlots{ .x = 0 },
    b: RegisterSlots = RegisterSlots{ .x = 0 },
    sp: u16 = 0,
    bp: u16 = 0,
    si: u16 = 0,
    di: u16 = 0,
    es: u16 = 0,
    cs: u16 = 0,
    ss: u16 = 0,
    ds: u16 = 0,
};

pub fn simulateAndPrintFile(filename: []u8, writer: anytype, alctr: std.mem.Allocator) !void {
    _ = alctr;
    _ = writer;
    _ = filename;
    // var file = try std.fs.cwd().openFile(filename, .{});
    // defer file.close();
    //
    // // for now, only support 1GB max.
    // const asm_bin = file.readToEndAlloc(alctr, 1024 * 1024 * 1024) catch |e| switch (e) {
    //     error.FileTooBig => {
    //         std.debug.print("Only input files up to 1GB are supported.\n", .{});
    //         return e;
    //     },
    //     else => return e,
    // };
    // defer alctr.free(asm_bin);
    //
    // try writer.print("simulation of {s}:\n", .{filename});
    //
    // var registers = Registers{};
    // var immediate_reg: u16 = 0;
    //
    // var decoder = dec.DecodeIterator.init(asm_bin);
    // while (decoder.next()) |inst| {
    //     switch (inst.mnemonic) {
    //         .mov => {
    //             const src = blk: {
    //                 std.debug.assert(inst.numOps() == 1);
    //                 switch (inst.source.op0.?) {
    //                     .imm => |i| {
    //                         immediate_reg = i.imm16;
    //                         break :blk &immediate_reg;
    //                     },
    //                     .none => return error.NoneOperand,
    //                     else => return error.Unimplemented,
    //                 }
    //             };
    //
    //             const dst = blk: {
    //                 std.debug.assert(inst.numOps() == 1);
    //                 switch (inst.source.op0.?) {
    //                     .reg => |r| {
    //                         break :blk registers
    //                     },
    //                     .none => return error.NoneOperand,
    //                     else => return error.Unimplemented,
    //                 }
    //             };
    //             _ = dst;
    //         }
    //     }
    // }
}

const std = @import("std");
const dec = @import("decode.zig");
