//! decoding machine language into assembly

/// 8086 register names. the order of the values in this enum has been chosen to match
/// table 4-9 in the 8086 users manual (page 263). the reg field is combined with the w
/// bit to make a number that maps to an enum value. see bitsToReg.
const Register = enum(u4) {
    // zig fmt: off
    al, cl, dl, bl, // a, b, c, d low. (byte)
    ah, ch, dh, bh, // a, b, c, d high. (byte)
    ax, cx, dx, bx, // a, b, c, d wide. (word, aka two bytes)
    sp, bp, si, di, // stack pointer, base pointer, source and destination. (word)
    // zig fmt: on
};

/// Instruction encoded immediate value.
const Immediate = union(enum) {
    imm8: i8,
    imm16: i16,
};

/// An instruction operand. In practice, instructions can involve 3 operands for a single
/// source or destination (see table 4-20 and the Instruction struct). To simplify
/// instruction representation, we assume every source or destination involves 3 operands.
/// When they in reality involve less, we fill in the buffer with `none`s.
const Operand = union(enum) {
    none: void,
    reg: Register,
    imm: Immediate,
};

/// Assembly mnemonic. These should match the assembly language exactly, as the printing
/// logic uses @tagName to get the strings at print time.
const Mnemonic = enum {
    mov,
    unknown,
};

// TODO consider merging Register and Immediate

/// Convert reg bits and the w flag to a register name.
fn bitsToReg(reg: u3, w: u1) Register {
    // see table 4-9 in the 8086 users manual. we combine the bits into a 4 bit number,
    // taking w to be the most significant bit
    var i: u4 = w;
    i <<= 3;
    i |= reg;
    return @intToEnum(Register, i);
}

/// The main instruction struct. Each instruction is decoded into an instance of this.
const Instruction = struct {
    mnemonic: Mnemonic,
    /// Destination operand(s). Some instructions only make use of 1 or 2 operands, in
    /// which case the decoder will fill the buffer with `Operand.none`.
    destination: [3]Operand,
    /// Source operands(s). Uses the same `none` packing logic as `destination`.
    source: [3]Operand,
    /// Size of the operand in machine code.
    encoded_bytes: u8,
    binary_index: usize,

    /// Many instructions share a common encoding scheme:
    ///
    /// [ byte0 ] [ mod/reg/rm ] [ disp lo ] [ disp hi ] [ data lo ] [ data hi ]
    /// disp and data are optional.
    ///
    /// byte0 will encode the mnemonic, but the operand data is packed basically the same way.
    /// This function takes between 2 and 6 bytes and generates an `Instruction` by
    /// interpreting them as being in the "6 Arithmetic" pattern.
    pub fn initFrom6Arith(bytes: []const u8, idx: usize) Instruction {
        std.debug.assert(bytes.len >= 2 and bytes.len <= 6);

        var parsed_len: u8 = 0;
        var src_ptr: *const [3]Operand = undefined; // set in block below
        var dst_ptr: *const [3]Operand = undefined; // set in block below

        // TODO fix up the weird control flow of this function. we are probably abusing defer
        {
            // if true, there is an immediate value on the end ([ data lo ] [ data hi])
            //
            // TODO maybe we can break this down even further. it would be nice to
            // have to seperate branches for has_immediate and !has_immediate, since
            // they *are* different. for now, the benefit of sharing their very
            // similar decoding logic outweights the readability penalty of having a
            // flag at the top of the function that can subtly change logic... i hope.
            const byte0 = bytes[0];
            const has_immediate = byte0 & 0b11111110 == 0b11000110;

            const d = @truncate(u1, byte0 >> 1);
            const w = @truncate(u1, byte0);

            // [ mod/reg/rm ]
            const byte1 = bytes[1];
            const mod = @truncate(u2, byte1 >> 6);
            const reg = @truncate(u3, byte1 >> 3);
            const rm = @truncate(u3, byte1);

            // all cases read at least 2 bytes
            defer parsed_len += 2;

            if (mod == 0b11) {
                // simpler reg to reg case. we assume that this never happens for imm
                // to reg, since that should be encoded differently as byte0 = [ 1011wreg ],
                // handled in a different switch branch.
                std.debug.assert(!has_immediate);

                const dst = if (d == 1) bitsToReg(reg, w) else bitsToReg(rm, w);
                const src = if (d == 1) bitsToReg(rm, w) else bitsToReg(reg, w);

                const dst_op = Operand{ .reg = dst };
                const src_op = Operand{ .reg = src };

                return Instruction{
                    .mnemonic = .mov,
                    .destination = makeSrcDst(1, .{dst_op}),
                    .source = makeSrcDst(1, .{src_op}),
                    .encoded_bytes = 2,
                    .binary_index = idx,
                };
            }

            // now we're in the rem/mem/imm to reg/mem w/ possible displacement case

            // assume that the location described in [mod/reg/rm] and [displo/hi] is
            // the destination for now. if the d bit is set, we will swap them at the
            // very end.

            // get the displacement, if applicable
            const displacement = blk: {
                if (mod == 0b00 and rm != 0b110) {
                    // instruction is two bytes long w/ no displacement, no action
                    // needed here.
                    break :blk Operand{ .none = {} };
                } else if (mod == 0b01) {
                    // byte displacement. these are treated as signed integers and
                    // are sign extended (handled implicitly) to an i16 for computation.
                    const byte2 = bytes[0 + 2];
                    break :blk Operand{ .imm = .{ .imm8 = @bitCast(i8, byte2) } };
                } else if (mod == 0b10 or (mod == 0b00 and rm == 0b110)) {
                    // word displacement or the special 0b11 case:
                    // a direct address mov with a two byte operand
                    const byte2 = bytes[0 + 2];
                    const byte3 = bytes[0 + 3];
                    break :blk Operand{ .imm = .{ .imm16 = (@as(i16, byte3) << 8) | byte2 } };
                } else {
                    unreachable;
                }
            };

            const displacement_size: u8 = switch (displacement) {
                .none => 0,
                .imm => |i| switch (i) {
                    .imm8 => 1,
                    .imm16 => 2,
                },
                else => unreachable,
            };
            defer parsed_len += displacement_size;

            // now we can finally get the destination from [mod/reg/rm] and [displo]/[disphi]
            const dst = bitsToSrcDst(rm, mod, displacement);

            // now get the source. again, for now, we assume that the source is the
            // reg/mem in [mod/reg/rm] or the immediate in the case of has_immediate.
            // we will check the d bit and swap as necessary at the end.

            const src_op = blk: {
                if (has_immediate) {
                    var byte_low: u8 = bytes[0 + 2 + displacement_size];
                    var byte_high: u8 = if (w == 1) bytes[0 + 2 + displacement_size + 1] else undefined;

                    break :blk if (w == 0)
                        Operand{ .imm = .{ .imm8 = @bitCast(i8, byte_low) } }
                    else
                        Operand{ .imm = .{ .imm16 = byte_low | (@as(i16, byte_high) << 8) } };
                } else {
                    break :blk Operand{ .reg = bitsToReg(reg, w) };
                }
            };

            const src_size: u8 = switch (src_op) {
                .imm => @as(u8, 1) + w,
                .reg => 0,
                else => unreachable,
            };
            defer parsed_len += src_size;

            const src = makeSrcDst(1, .{src_op});

            // swap src and dst if the d bit is not set. watch out, in imm to reg/mem,
            // the d bit is always 1.
            src_ptr = &src;
            dst_ptr = &dst;

            if (d == 1 and !has_immediate) std.mem.swap(*const [3]Operand, &src_ptr, &dst_ptr);
        }

        return Instruction{
            .mnemonic = .mov,
            .destination = dst_ptr.*,
            .source = src_ptr.*,
            .encoded_bytes = parsed_len,
            .binary_index = idx,
        };
    }
};

fn makeSrcDst(comptime num: u8, inits: [num]Operand) [3]Operand {
    const none = Operand{ .none = {} };
    var result: [3]Operand = .{ none, none, none };
    inline for (0..num) |i| {
        result[i] = inits[i];
    }
    return result;
}

/// Iterate through a byte buffer and produce `Instruction`s.
const DecodeIterator = struct {
    bytes: []const u8,
    index: usize = 0,

    pub fn init(bytes: []const u8) DecodeIterator {
        return DecodeIterator{
            .bytes = bytes,
            .index = 0,
        };
    }

    /// Decode the next instruction in the buffer. return null at the end.
    pub fn next(self: *DecodeIterator) ?Instruction {
        const starting_index = self.index;
        if (self.index >= self.bytes.len) return null;

        const byte0 = self.bytes[self.index];
        const end = @min(self.bytes.len, self.index + 6);

        switch (byte0) {
            0b10001000...0b10001011, // reg/mem to/from reg, 100010dw
            0b11000110...0b11000111, // imm to reg/mem, 1100011w
            => {
                const i = Instruction.initFrom6Arith(self.bytes[self.index..end], self.index);
                defer self.index += i.encoded_bytes;
                return i;
            },

            // imm to reg
            // 1011wreg
            0b10110000...0b10111111 => {
                const w = @truncate(u1, byte0 >> 3);
                const reg_bits = @truncate(u3, byte0);
                defer self.index += @as(u8, 2) + w;
                const byte1 = self.bytes[starting_index + 1];
                const imm = blk: {
                    const byte2 = if (w == 1) self.bytes[starting_index + 2] else undefined;
                    break :blk if (w == 0) Operand{ .imm = .{ .imm8 = @bitCast(i8, byte1) } } else Operand{ .imm = .{ .imm16 = (@as(i16, byte2) << 8) | byte1 } };
                };

                const reg = bitsToReg(reg_bits, w);
                const reg_op = Operand{ .reg = reg };

                return Instruction{
                    .mnemonic = .mov,
                    .destination = makeSrcDst(1, .{reg_op}),
                    .source = makeSrcDst(1, .{imm}),
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

                const reg: Register = if (w == 0) .al else .ax;
                const reg_ops: [3]Operand = .{ .{ .reg = reg }, .{ .none = {} }, .{ .none = {} } };

                const addr8 = byte1;
                const addr16 = (@as(u16, byte2) << 8) | addr8;

                const mem_ops = if (w == 0)
                    makeSrcDst(2, .{
                        .{ .imm = .{ .imm8 = @bitCast(i8, addr8) } },
                        .{ .imm = .{ .imm8 = 0 } },
                    })
                else
                    makeSrcDst(2, .{
                        .{ .imm = .{ .imm16 = @bitCast(i16, addr16) } },
                        .{ .imm = .{ .imm8 = 0 } },
                    });

                var dst = &reg_ops;
                var src = &mem_ops;
                if (not_d == 1) std.mem.swap(*const [3]Operand, &dst, &src);

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
                const none = Operand{ .none = {} };

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

fn bitsToSrcDst(reg_or_mem: u3, mod: u2, imm_value: Operand) [3]Operand {
    // mod == 0b11 is a reg to reg move and has no displacement calculation
    std.debug.assert(mod != 0b11);

    // see table 4-10. we combine the bits into a 5 bit number, taking mod to be the most
    // significant bits
    var i: u5 = mod;
    i <<= 3;
    i |= reg_or_mem;

    const none = Operand{ .none = {} };

    // TODO: tame this beast

    return switch (i) {
        // mod == 0b00
        0b00_000 => .{ .{ .reg = .bx }, .{ .reg = .si }, none },
        0b00_001 => .{ .{ .reg = .bx }, .{ .reg = .di }, none },
        0b00_010 => .{ .{ .reg = .bp }, .{ .reg = .si }, none },
        0b00_011 => .{ .{ .reg = .bp }, .{ .reg = .di }, none },
        // these have an immediate zero in them so the printer prints them as effective
        // address calculations.
        0b00_100 => .{ .{ .reg = .si }, .{ .imm = .{ .imm8 = 0 } }, none },
        0b00_101 => .{ .{ .reg = .di }, .{ .imm = .{ .imm8 = 0 } }, none },
        0b00_110 => .{ imm_value, .{ .imm = .{ .imm8 = 0 } }, none },
        0b00_111 => .{ .{ .reg = .bx }, .{ .imm = .{ .imm8 = 0 } }, none },

        // mod == 0b01
        0b01_000 => .{ .{ .reg = .bx }, .{ .reg = .si }, imm_value },
        0b01_001 => .{ .{ .reg = .bx }, .{ .reg = .di }, imm_value },
        0b01_010 => .{ .{ .reg = .bp }, .{ .reg = .si }, imm_value },
        0b01_011 => .{ .{ .reg = .bp }, .{ .reg = .di }, imm_value },
        0b01_100 => .{ .{ .reg = .si }, imm_value, none },
        0b01_101 => .{ .{ .reg = .di }, imm_value, none },
        0b01_110 => .{ .{ .reg = .bp }, imm_value, none },
        0b01_111 => .{ .{ .reg = .bx }, imm_value, none },

        // mod == 0b10
        0b10_000 => .{ .{ .reg = .bx }, .{ .reg = .si }, imm_value },
        0b10_001 => .{ .{ .reg = .bx }, .{ .reg = .di }, imm_value },
        0b10_010 => .{ .{ .reg = .bp }, .{ .reg = .si }, imm_value },
        0b10_011 => .{ .{ .reg = .bp }, .{ .reg = .di }, imm_value },
        0b10_100 => .{ .{ .reg = .si }, imm_value, none },
        0b10_101 => .{ .{ .reg = .di }, imm_value, none },
        0b10_110 => .{ .{ .reg = .bp }, imm_value, none },
        0b10_111 => .{ .{ .reg = .bx }, imm_value, none },

        else => unreachable,
    };
}

/// Interpret `ops` as the source/destination operands of some instruction. Compose the
/// operands into a single string that is appropriate for the source/destination of some
/// instruction and write the string to `writer`.
fn writeInstSrcDst(ops: [3]Operand, writer: anytype) !void {
    const num_ops = blk: {
        var i: usize = 0;
        for (ops) |op| {
            if (std.meta.activeTag(op) == .none) break;
            i += 1;
        }
        break :blk i;
    };

    if (num_ops == 1) {
        switch (ops[0]) {
            .none => {},
            .reg => |r| try writer.print("{s}", .{@tagName(r)}),
            .imm => |iv| {
                switch (iv) {
                    .imm8 => try writer.print("byte {}", .{iv.imm8}),
                    .imm16 => try writer.print("word {}", .{iv.imm16}),
                }
            },
        }
    } else {
        try writer.print("[", .{});
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
        try writer.print("]", .{});
    }
}

/// Given `filename`, interpret it as 8086 machine code and write the corresponding 8086
/// assembly to `writer`.
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
        try writeInstSrcDst(inst.destination, writer);
        try writer.print(", ", .{});
        try writeInstSrcDst(inst.source, writer);
        try writer.print("\n\n", .{});
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

/// Run an end to end decoder test. Given a "basename" that represents a file,
/// 1. Compile ./asm/<basename>.asm to a ./testfs/<basename> binary using nasm
/// 2. Decode the ./asm/<basename> binary with our decoder and write the decoded assembly
///    to ./testfs/<basename>.asm
/// 3. Compile the ./testfs/<basename>.asm to a ./testfs/<basename>2 binary using nasm
/// 4. Assert that the ./testfs/<basename> and ./testfs/<basename>2 binaries are identical
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

test "e2e 0040" {
    const alctr = std.testing.allocator;
    try e2eTest("listing_0040_challenge_movs", alctr);
}

test "e2e 0040 positive" {
    const alctr = std.testing.allocator;
    try e2eTest("listing_0040_challenge_movs_positive", alctr);
}

test "e2e negative displacement" {
    const alctr = std.testing.allocator;
    try e2eTest("negative_mov", alctr);
}

// test "e2e 0041" {
//     const alctr = std.testing.allocator;
//     try e2eTest("listing_0041_add_sub_cmp_jnz", alctr);
// }

// test "e2e 0042" {
//     const alctr = std.testing.allocator;
//     try e2eTest("listing_0042_completionist_decode", alctr);
// }

const std = @import("std");
const expectEq = std.testing.expectEqual;
const expect = std.testing.expect;
