//! decoding machine language into assembly

/// 8086 register names. the order of the values in this enum has been chosen to match
/// table 4-9 in the 8086 users manual (page 263). the reg field is combined with the w
/// bit to make a number that maps to an enum value. see bitsToReg.
const Register = enum(u4) {
    al,
    cl,
    dl,
    bl,
    ah,
    ch,
    dh,
    bh,
    ax,
    cx,
    dx,
    bx,
    sp,
    bp,
    si,
    di,
};

const Immediate = enum(u4) {
    imm8,
    imm16,
};

/// convert reg bits and the w flag to a register name
fn bitsToReg(reg: u3, w: u1) Register {
    // see table 4-9 in the 8086 users manual. we combine the bits into a 4 bit number,
    // taking w to be the most significant bit
    var i: u4 = w;
    i <<= 3;
    i |= reg;
    return @intToEnum(Register, i);
}

const SumOperand = union(enum) {
    none: void,
    reg: Register,
    imm: Immediate,
};

fn bitsToSum(reg_or_mem: u3, mod: u2) [3]SumOperand {
    // mod == 0b11 is a reg to reg move and has no displacement calculation
    std.debug.assert(mod != 0b11);

    // see table 4-10. we combine the bits into a 5 bit number, taking mod to be the most
    // significant bits
    var i: u5 = mod;
    i <<= 3;
    i |= reg_or_mem;

    const SO = SumOperand;

    return switch (i) {
        // mod == 0b00
        0b00_000 => .{ SO{ .reg = .bx }, SO{ .reg = .si }, SO{ .none = {} } },
        0b00_001 => .{ SO{ .reg = .bx }, SO{ .reg = .di }, SO{ .none = {} } },
        0b00_010 => .{ SO{ .reg = .bp }, SO{ .reg = .si }, SO{ .none = {} } },
        0b00_011 => .{ SO{ .reg = .bp }, SO{ .reg = .di }, SO{ .none = {} } },
        0b00_100 => .{ SO{ .reg = .si }, SO{ .none = {} }, SO{ .none = {} } },
        0b00_101 => .{ SO{ .reg = .di }, SO{ .none = {} }, SO{ .none = {} } },
        0b00_110 => .{ SO{ .imm = .imm16 }, SO{ .none = {} }, SO{ .none = {} } },
        0b00_111 => .{ SO{ .reg = .bx }, SO{ .none = {} }, SO{ .none = {} } },

        // mod == 0b01
        0b01_000 => .{ SO{ .reg = .bx }, SO{ .reg = .si }, SO{ .imm = .imm8 } },
        0b01_001 => .{ SO{ .reg = .bx }, SO{ .reg = .di }, SO{ .imm = .imm8 } },
        0b01_010 => .{ SO{ .reg = .bp }, SO{ .reg = .si }, SO{ .imm = .imm8 } },
        0b01_011 => .{ SO{ .reg = .bp }, SO{ .reg = .di }, SO{ .imm = .imm8 } },
        0b01_100 => .{ SO{ .reg = .si }, SO{ .imm = .imm8 }, SO{ .none = {} } },
        0b01_101 => .{ SO{ .reg = .di }, SO{ .imm = .imm8 }, SO{ .none = {} } },
        0b01_110 => .{ SO{ .reg = .bp }, SO{ .imm = .imm8 }, SO{ .none = {} } },
        0b01_111 => .{ SO{ .reg = .bx }, SO{ .imm = .imm8 }, SO{ .none = {} } },

        // mod == 0b10
        0b10_000 => .{ SO{ .reg = .bx }, SO{ .reg = .si }, SO{ .imm = .imm16 } },
        0b10_001 => .{ SO{ .reg = .bx }, SO{ .reg = .di }, SO{ .imm = .imm16 } },
        0b10_010 => .{ SO{ .reg = .bp }, SO{ .reg = .si }, SO{ .imm = .imm16 } },
        0b10_011 => .{ SO{ .reg = .bp }, SO{ .reg = .di }, SO{ .imm = .imm16 } },
        0b10_100 => .{ SO{ .reg = .si }, SO{ .imm = .imm16 }, SO{ .none = {} } },
        0b10_101 => .{ SO{ .reg = .di }, SO{ .imm = .imm16 }, SO{ .none = {} } },
        0b10_110 => .{ SO{ .reg = .bp }, SO{ .imm = .imm16 }, SO{ .none = {} } },
        0b10_111 => .{ SO{ .reg = .bx }, SO{ .imm = .imm16 }, SO{ .none = {} } },

        else => unreachable,
    };
}

fn writeDisplacement(ops: [3]SumOperand, immediate_value: i16, writer: anytype) !void {
    const num_ops = blk: {
        var i: usize = 0;
        for (ops) |op| {
            if (std.meta.activeTag(op) == .none) break;
            i += 1;
        }
        break :blk i;
    };
    std.debug.assert(num_ops > 0);

    try writer.print("[ ", .{});

    for (ops, 0..) |op, i| {
        switch (op) {
            .none => {},
            .reg => |r| {
                // TODO offset might be negative
                if (i > 0) try writer.print("+ ", .{});
                try writer.print("{s} ", .{@tagName(r)});
            },
            .imm => {
                // TODO offset might be negative
                if (i > 0) try writer.print("+ ", .{});
                // TODO print in hex with dec in comment
                try writer.print("{} ", .{immediate_value});
            },
        }
    }

    try writer.print("]", .{});
}

pub fn decodeAndPrintFile(filename: []const u8, writer: anytype, alctr: std.mem.Allocator) !void {
    var file = try std.fs.cwd().openFile(filename, .{});
    defer file.close();

    // for now, only support 1GB max.
    const asm_bin = file.readToEndAlloc(alctr, 1024 * 1024 * 1024) catch |e| switch (e) {
        error.FileTooBig => {
            std.debug.print("Only input files up to 1GB are supported.\n", .{});
            return e;
        },
        else => return e,
    };
    defer alctr.free(asm_bin);

    try writer.print("; disassembly of {s}:\n", .{filename});
    try writer.print("bits 16\n\n", .{});

    var i: usize = 0;
    while (i < asm_bin.len) : (i += 1) {
        const byte0 = asm_bin[i];

        { // check for a 4 bit opcode
            const opcode_hi_4 = @truncate(u4, byte0 >> 4);
            switch (opcode_hi_4) {
                0b1011 => {
                    const w = @truncate(u1, byte0 >> 3);
                    const reg = @truncate(u3, byte0);

                    i += 1;
                    const byte1 = asm_bin[i];

                    const imm_value = blk: {
                        const byte2 = iblk: {
                            if (w == 1) {
                                i += 1;
                                break :iblk asm_bin[i];
                            }
                            break :iblk 0;
                        };

                        break :blk byte1 | (@as(u16, byte2) << 8);
                    };

                    const reg_name = @tagName(bitsToReg(reg, w));
                    try writer.print("mov {s}, {}\n", .{ reg_name, imm_value });

                    continue;
                },
                else => {}, // fall through to 6 bit opcodes
            }
        }

        { // check for a 6 bit opcode
            const opcode_hi_6 = @truncate(u6, byte0 >> 2);
            switch (opcode_hi_6) {
                0b100010 => {
                    // reg/mem to/from reg. see table 4-10
                    const d = @truncate(u1, byte0 >> 1);
                    const w = @truncate(u1, byte0);

                    i += 1;
                    const byte1 = asm_bin[i];

                    const mod = @truncate(u2, byte1 >> 6);
                    const reg = @truncate(u3, byte1 >> 3);
                    const reg_or_mem = @truncate(u3, byte1);

                    switch (mod) {
                        0b00, 0b01, 0b10 => {
                            const formula = bitsToSum(reg_or_mem, mod);
                            const reg_name = @tagName(bitsToReg(reg, w));

                            try writer.print("mov ", .{});

                            if (d == 1) {
                                try writer.print("{s}, ", .{reg_name});
                            }

                            const imm_value: i16 = blk: {
                                if (mod == 0b00 and reg_or_mem != 0b110) {
                                    // instruction is two bytes long w/ no displacement, no action
                                    // needed here. imm_value will go unused.
                                    break :blk 0;
                                } else if (mod == 0b01) {
                                    // byte displacement. these are treated as signed integers and
                                    // are sign extended (handled implicitly) to an i16 for computation.
                                    i += 1;
                                    break :blk @bitCast(i8, asm_bin[i]);
                                } else if (mod == 0b10 or (mod == 0b00 and reg_or_mem == 0b110)) {
                                    // word displacement or the special 0b11 case:
                                    // a direct address mov with a two byte operand
                                    i += 1;
                                    const byte2 = asm_bin[i];
                                    i += 1;
                                    const byte3 = asm_bin[i];
                                    break :blk byte2 | (@as(i16, byte3) << 8);
                                } else {
                                    try writer.print("??? {b} {b}\n", .{ mod, reg_or_mem });
                                    unreachable;
                                }
                            };

                            try writeDisplacement(formula, imm_value, writer);

                            if (d == 0) {
                                try writer.print(", {s}", .{reg_name});
                            }

                            try writer.print("\n", .{});
                        },
                        0b11 => {
                            const dst = if (d == 1) bitsToReg(reg, w) else bitsToReg(reg_or_mem, w);
                            const src = if (d == 1) bitsToReg(reg_or_mem, w) else bitsToReg(reg, w);

                            try writer.print("; 0b{b:0<8} 0b{b:0<8}\n", .{ byte0, byte1 });
                            try writer.print("mov {s}, {s}\n", .{ @tagName(dst), @tagName(src) });
                        },
                    }
                },
                else => {
                    try writer.print("; warning: unknown opcode byte 0b{b:0<8}\n", .{byte0});
                },
            }
        }
    }
}

test "bitsToReg" {
    // from table 4-9 in intel 8086 users manual

    try expectEq(Register.al, bitsToReg(0b000, 0));
    try expectEq(Register.cl, bitsToReg(0b001, 0));
    try expectEq(Register.dl, bitsToReg(0b010, 0));
    try expectEq(Register.bl, bitsToReg(0b011, 0));
    try expectEq(Register.ah, bitsToReg(0b100, 0));
    try expectEq(Register.ch, bitsToReg(0b101, 0));
    try expectEq(Register.dh, bitsToReg(0b110, 0));
    try expectEq(Register.bh, bitsToReg(0b111, 0));

    try expectEq(Register.ax, bitsToReg(0b000, 1));
    try expectEq(Register.cx, bitsToReg(0b001, 1));
    try expectEq(Register.dx, bitsToReg(0b010, 1));
    try expectEq(Register.bx, bitsToReg(0b011, 1));
    try expectEq(Register.sp, bitsToReg(0b100, 1));
    try expectEq(Register.bp, bitsToReg(0b101, 1));
    try expectEq(Register.si, bitsToReg(0b110, 1));
    try expectEq(Register.di, bitsToReg(0b111, 1));
}

fn e2eTest(comptime basename: []const u8, alctr: std.mem.Allocator) !void {
    // compile reference asm
    {
        const res = try std.ChildProcess.exec(.{
            .allocator = alctr,
            .argv = &.{ "nasm", "./asm/" ++ basename ++ ".asm", "-o", "./testfs/" ++ basename },
        });
        defer alctr.free(res.stdout);
        defer alctr.free(res.stderr);

        errdefer std.debug.print("stdout:\n{s}\n", .{res.stdout});
        errdefer std.debug.print("stderr:\n{s}\n", .{res.stderr});
        try expectEq(@as(u8, res.term.Exited), 0);
    }

    // decode binary
    {
        const decoded = try std.fs.cwd().createFile("./testfs/" ++ basename ++ ".asm", .{});
        defer decoded.close();
        try decodeAndPrintFile("./testfs/" ++ basename, decoded.writer(), alctr);
    }

    // compile decoded asm
    {
        const res = try std.ChildProcess.exec(.{
            .allocator = alctr,
            .argv = &.{ "nasm", "./testfs/" ++ basename ++ ".asm", "-o", "./testfs/" ++ basename ++ "2" },
        });
        defer alctr.free(res.stdout);
        defer alctr.free(res.stderr);

        errdefer std.debug.print("stdout:\n{s}\n", .{res.stdout});
        errdefer std.debug.print("stderr:\n{s}\n", .{res.stderr});
        try expectEq(@as(u8, res.term.Exited), 0);
    }

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

test "e2e 0037" {
    const alctr = std.testing.allocator;
    try e2eTest("listing_0037_single_register_mov", alctr);
}

test "e2e 0038" {
    const alctr = std.testing.allocator;
    try e2eTest("listing_0038_many_register_mov", alctr);
}

test "e2e 0039" {
    const alctr = std.testing.allocator;
    try e2eTest("listing_0039_more_movs", alctr);
}

// test "e2e 0040" {
//     const alctr = std.testing.allocator;
//     try e2eTest("listing_0040_challenge_movs", alctr);
// }

test "e2e negative displacement" {
    const alctr = std.testing.allocator;
    try e2eTest("negative_mov", alctr);
}

const std = @import("std");
const expectEq = std.testing.expectEqual;
const expect = std.testing.expect;
