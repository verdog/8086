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

const Immediate = union(enum) {
    imm8: i8,
    imm16: i16,
};

const SumOperand = union(enum) {
    none: void,
    reg: Register,
    imm: Immediate,
};

const Mnemonic = enum {
    mov,
    unknown,
};

// TODO consider merging Register and Immediate

/// convert reg bits and the w flag to a register name
fn bitsToReg(reg: u3, w: u1) Register {
    // see table 4-9 in the 8086 users manual. we combine the bits into a 4 bit number,
    // taking w to be the most significant bit
    var i: u4 = w;
    i <<= 3;
    i |= reg;
    return @intToEnum(Register, i);
}

const Instruction = struct {
    mnemonic: Mnemonic,
    destination: [3]SumOperand,
    source: [3]SumOperand,
    encoded_bytes: u8,
    binary_index: usize,
};

const DecodeIterator = struct {
    bytes: []const u8,
    index: usize = 0,

    pub fn init(bytes: []const u8) DecodeIterator {
        return DecodeIterator{
            .bytes = bytes,
            .index = 0,
        };
    }

    /// decode the next instruction in the buffer. return null at the end
    pub fn next(self: *DecodeIterator) ?Instruction {
        const starting_index = self.index;
        if (self.index >= self.bytes.len) return null;

        const byte0 = self.bytes[self.index];
        switch (byte0) {
            // reg/mem to/from reg
            // 100010dw
            0b10001000...0b10001011 => {
                const d = @truncate(u1, byte0 >> 1);
                const w = @truncate(u1, byte0);

                const byte1 = self.bytes[self.index + 1];

                const mod = @truncate(u2, byte1 >> 6);
                const reg_bits = @truncate(u3, byte1 >> 3);
                const rm = @truncate(u3, byte1);

                // all cases read at least 2 bytes
                defer self.index += 2;

                if (mod == 0b11) {
                    // simpler reg to reg case

                    const dst = if (d == 1) bitsToReg(reg_bits, w) else bitsToReg(rm, w);
                    const src = if (d == 1) bitsToReg(rm, w) else bitsToReg(reg_bits, w);

                    const dst_op = SumOperand{ .reg = dst };
                    const src_op = SumOperand{ .reg = src };
                    const none = SumOperand{ .none = {} };

                    return Instruction{
                        .mnemonic = .mov,
                        .destination = .{ dst_op, none, none },
                        .source = .{ src_op, none, none },
                        .encoded_bytes = 2,
                        .binary_index = self.index,
                    };
                }

                // now we're in the rem/mem to reg/mem w/ displacement case

                const none = SumOperand{ .none = {} };
                const reg: [3]SumOperand = .{ SumOperand{ .reg = bitsToReg(reg_bits, w) }, none, none };
                const imm_value = blk: {
                    if (mod == 0b00 and rm != 0b110) {
                        // instruction is two bytes long w/ no displacement, no action
                        // needed here.
                        break :blk none;
                    } else if (mod == 0b01) {
                        // byte displacement. these are treated as signed integers and
                        // are sign extended (handled implicitly) to an i16 for computation.
                        const byte2 = self.bytes[self.index + 2];
                        // TODO defer self.index += 1
                        break :blk SumOperand{ .imm = Immediate{ .imm8 = @bitCast(i8, byte2) } };
                    } else if (mod == 0b10 or (mod == 0b00 and rm == 0b110)) {
                        // word displacement or the special 0b11 case:
                        // a direct address mov with a two byte operand
                        // TODO defer self.index += 2
                        const byte2 = self.bytes[self.index + 2];
                        const byte3 = self.bytes[self.index + 3];
                        break :blk SumOperand{ .imm = Immediate{ .imm16 = byte2 | (@as(i16, byte3) << 8) } };
                    } else {
                        unreachable;
                    }
                };

                const additional_size: u8 = switch (imm_value) {
                    .none => 0,
                    .imm => |i| switch (i) {
                        .imm8 => 1,
                        .imm16 => 2,
                    },
                    else => unreachable,
                };
                defer self.index += additional_size;

                const formula = bitsToSum(rm, mod, imm_value);

                var dst = &formula;
                var src = &reg;

                if (d == 1) std.mem.swap(*const [3]SumOperand, &dst, &src);

                return Instruction{
                    .mnemonic = .mov,
                    .destination = dst.*,
                    .source = src.*,
                    .encoded_bytes = 2 + additional_size,
                    .binary_index = self.index,
                };
            },

            // imm to reg/mem
            // 1100011w
            // 0b11000110...0b11000111 => {
            // },

            // imm to reg
            // 1011wreg
            0b10110000...0b10111111 => {
                const w = @truncate(u1, byte0 >> 3);
                const reg_bits = @truncate(u3, byte0);
                defer self.index += @as(u8, 2) + w;

                const byte1 = self.bytes[starting_index + 1];

                const imm_value = blk: {
                    const byte2 = iblk: {
                        if (w == 1) {
                            break :iblk self.bytes[starting_index + 2];
                        }
                        break :iblk 0;
                    };

                    break :blk byte1 | (@as(i16, byte2) << 8);
                };

                const reg = bitsToReg(reg_bits, w);
                const reg_op = SumOperand{ .reg = reg };
                const value_op = SumOperand{ .imm = Immediate{ .imm16 = imm_value } };
                const none = SumOperand{ .none = {} };

                return Instruction{
                    .mnemonic = .mov,
                    .destination = .{ reg_op, none, none },
                    .source = .{ value_op, none, none },
                    .encoded_bytes = @as(u8, 2) + w,
                    .binary_index = self.index,
                };
            },

            // ax/mem to ax/mem
            // 101000dw
            0b10100000...0b10100011 => {
                // for some reason, the usual d logic is reversed for this instruction.
                // (it's not actually labeled as a d bit in the manual)
                const not_d = @truncate(u1, byte0 >> 1);
                const w = @truncate(u1, byte0);

                defer self.index += @as(u8, 2) + w;

                const byte1 = self.bytes[self.index + 1];
                const byte2 = self.bytes[self.index + 2];
                const none = SumOperand{ .none = {} };

                const reg: Register = if (w == 0) .al else .ax;
                const reg_ops: [3]SumOperand = .{ .{ .reg = reg }, .{ .none = {} }, .{ .none = {} } };

                const addr8 = byte1;
                const addr16 = (@as(u16, byte2) << 8) | addr8;

                const mem_ops: [3]SumOperand = if (w == 0)
                    .{ .{ .imm = .{ .imm8 = @bitCast(i8, addr8) } }, .{ .imm = .{ .imm8 = 0 } }, none }
                else
                    .{ .{ .imm = .{ .imm16 = @bitCast(i16, addr16) } }, .{ .imm = .{ .imm8 = 0 } }, none };

                var dst = &reg_ops;
                var src = &mem_ops;
                if (not_d == 1) std.mem.swap(*const [3]SumOperand, &dst, &src);

                return Instruction{
                    .mnemonic = .mov,
                    .destination = dst.*,
                    .source = src.*,
                    .encoded_bytes = @as(u8, 2) + w,
                    .binary_index = self.index,
                };
            },

            else => {
                defer self.index += 1;
                const none = SumOperand{ .none = {} };

                return Instruction{
                    .mnemonic = .unknown,
                    .destination = .{ none, none, none },
                    .source = .{ none, none, none },
                    .encoded_bytes = 1,
                    .binary_index = self.index,
                };
            },
        }
    }
};

fn bitsToSum(reg_or_mem: u3, mod: u2, imm_value: SumOperand) [3]SumOperand {
    // mod == 0b11 is a reg to reg move and has no displacement calculation
    std.debug.assert(mod != 0b11);

    // see table 4-10. we combine the bits into a 5 bit number, taking mod to be the most
    // significant bits
    var i: u5 = mod;
    i <<= 3;
    i |= reg_or_mem;

    const SO = SumOperand;
    const I = Immediate;
    const none = SumOperand{ .none = {} };

    // TODO: tame this beast

    return switch (i) {
        // mod == 0b00
        0b00_000 => .{ SO{ .reg = .bx }, SO{ .reg = .si }, none },
        0b00_001 => .{ SO{ .reg = .bx }, SO{ .reg = .di }, none },
        0b00_010 => .{ SO{ .reg = .bp }, SO{ .reg = .si }, none },
        0b00_011 => .{ SO{ .reg = .bp }, SO{ .reg = .di }, none },
        // these have an immediate zero in them so the printer prints them as effective
        // address calculations.
        0b00_100 => .{ SO{ .reg = .si }, SO{ .imm = I{ .imm8 = 0 } }, none },
        0b00_101 => .{ SO{ .reg = .di }, SO{ .imm = I{ .imm8 = 0 } }, none },
        0b00_110 => .{ imm_value, SO{ .imm = I{ .imm8 = 0 } }, none },
        0b00_111 => .{ SO{ .reg = .bx }, SO{ .imm = I{ .imm8 = 0 } }, none },

        // mod == 0b01
        0b01_000 => .{ SO{ .reg = .bx }, SO{ .reg = .si }, imm_value },
        0b01_001 => .{ SO{ .reg = .bx }, SO{ .reg = .di }, imm_value },
        0b01_010 => .{ SO{ .reg = .bp }, SO{ .reg = .si }, imm_value },
        0b01_011 => .{ SO{ .reg = .bp }, SO{ .reg = .di }, imm_value },
        0b01_100 => .{ SO{ .reg = .si }, imm_value, none },
        0b01_101 => .{ SO{ .reg = .di }, imm_value, none },
        0b01_110 => .{ SO{ .reg = .bp }, imm_value, none },
        0b01_111 => .{ SO{ .reg = .bx }, imm_value, none },

        // mod == 0b10
        0b10_000 => .{ SO{ .reg = .bx }, SO{ .reg = .si }, imm_value },
        0b10_001 => .{ SO{ .reg = .bx }, SO{ .reg = .di }, imm_value },
        0b10_010 => .{ SO{ .reg = .bp }, SO{ .reg = .si }, imm_value },
        0b10_011 => .{ SO{ .reg = .bp }, SO{ .reg = .di }, imm_value },
        0b10_100 => .{ SO{ .reg = .si }, imm_value, none },
        0b10_101 => .{ SO{ .reg = .di }, imm_value, none },
        0b10_110 => .{ SO{ .reg = .bp }, imm_value, none },
        0b10_111 => .{ SO{ .reg = .bx }, imm_value, none },

        else => unreachable,
    };
}

// TODO rename. this is used as a general purpose lhs/rhs printing function now.
fn writeDisplacement(ops: [3]SumOperand, writer: anytype) !void {
    const num_ops = blk: {
        var i: usize = 0;
        for (ops) |op| {
            if (std.meta.activeTag(op) == .none) break;
            i += 1;
        }
        break :blk i;
    };

    if (num_ops > 1) try writer.print("[", .{});

    for (ops, 0..) |op, i| {
        switch (op) {
            .none => {},
            .reg => |r| {
                if (i > 0) try writer.print("+", .{});
                try writer.print("{s}", .{@tagName(r)});
            },
            .imm => |ival| {
                // TODO offset might be negative
                if (i > 0) try writer.print("+", .{});
                // TODO print in hex with dec in comment
                const pval = if (std.meta.activeTag(ival) == .imm8) @intCast(i16, ival.imm8) else ival.imm16;
                try writer.print("{}", .{pval});
            },
        }
    }

    if (num_ops > 1) try writer.print("]", .{});
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

    var instructions = std.ArrayList(Instruction).init(alctr);
    defer instructions.deinit();

    var decoder = DecodeIterator.init(asm_bin);
    while (decoder.next()) |inst| {
        try instructions.append(inst);
    }

    for (instructions.items) |inst| {
        // binary comment
        try writer.print(";", .{});
        for (0..inst.encoded_bytes) |i| {
            try writer.print(" {b:0>8}", .{asm_bin[inst.binary_index + i]});
        }

        // instruction
        try writer.print("\n{s} ", .{@tagName(inst.mnemonic)});
        try writeDisplacement(inst.destination, writer);
        try writer.print(", ", .{});
        try writeDisplacement(inst.source, writer);
        try writer.print("\n\n", .{});
    }

    // var i: usize = 0;
    // while (i < asm_bin.len) : (i += 1) {
    //     const byte0 = asm_bin[i];
    //
    //     { // check for a 6 bit opcode
    //         const opcode_hi_6 = @truncate(u6, byte0 >> 2);
    //         switch (opcode_hi_6) {
    //             0b100010 => {
    //                 }
    //             },
    //             else => {}, // fall through to 7 bit opcodes
    //         }
    //     }
    //
    //     { // check for a 7 bit opcode
    //         const opcode_hi_7 = @truncate(u7, byte0 >> 1);
    //         switch (opcode_hi_7) {
    //             0b1100011 => {
    //                 // immediate to register/memory
    //                 const w = @truncate(u1, byte0);
    //                 _ = w;
    //
    //                 i += 1;
    //                 const byte1 = asm_bin[i];
    //
    //                 const mod = @truncate(u2, byte1 >> 6);
    //                 _ = mod;
    //                 const reg_or_mem = @truncate(u3, byte1);
    //                 _ = reg_or_mem;
    //             },
    //             else => try writer.print("; warning: unknown opcode byte 0b{b:0<8}\n", .{byte0}),
    //         }
    //     }
    // }
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
