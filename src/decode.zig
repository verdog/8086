//! decoding machine language into assembly

/// Instruction encoded immediate value.
/// TODO fold this up into Operand
const Immediate = union(enum) {
    inst_addr: i8, // instruction address. displayed differently (as labels).
    imm16: i16,
};

/// An instruction operand. In practice, instructions can involve 3 operands for a single
/// source or destination (see table 4-20 and the Instruction struct). To simplify
/// instruction representation, we assume every source or destination involves 3 operands.
/// When they in reality involve less, we fill in the buffer with `none`s.
pub const Operand = union(enum) {
    none: void,
    reg: nms.Register,
    imm: Immediate,
};

/// Convert reg bits and the w flag to a register name.
fn bitsToReg(reg: u3, w: u1) nms.Register {
    // see table 4-9 in the 8086 users manual. we combine the bits into a 4 bit number,
    // taking w to be the most significant bit
    var i: u4 = w;
    i <<= 3;
    i |= reg;
    return @intToEnum(nms.Register, i);
}

/// Convert reg bits to segment register
fn bitsToSegReg(reg: u2) nms.Register {
    return @intToEnum(nms.Register, @enumToInt(nms.Register.es) + reg);
}

/// Convert prefix bits to Prefix
fn bitsToPrefix(byte: u8) Prefix {
    return switch (byte) {
        0b11110010...0b11110011 => .{ .cnst = .rep },
        0b11110000 => .{ .cnst = .lock },
        0b00100110 => .{ .seg = .es },
        0b00101110 => .{ .seg = .cs },
        0b00110110 => .{ .seg = .ss },
        0b00111110 => .{ .seg = .ds },
        else => .{ .none = {} },
    };
}

fn bitsToMnemonic(byte: u8) nms.Mnemonic {
    // see table 4-12
    return switch (byte) {
        0b10001000...0b10001011, // mov reg/mem to/from reg
        0b11000110...0b11000111, // mov imm to reg/mem
        0b10110000...0b10111111, // mov imm to reg
        0b10100000...0b10100011, // mov ax/mem to ax/mem
        0b10001110, // reg/mem to segreg
        0b10001100, // segreg to reg/mem
        => return .mov,

        0b00000000...0b00000011, // add reg/mem with reg to reg/mem
        0b00000100...0b00000101, // add imm to ax
        => return .add,

        0b00010000...0b00010011, // adc reg/mem with reg to reg/mem
        0b00010100...0b00010101, // add imm to ax
        => return .adc,

        0b01000000...0b01000111, // inc register
        => return .inc,

        0b00110111 => return .aaa,
        0b00100111 => return .daa,
        0b00111111 => return .aas,
        0b00101111 => return .das,
        0b11010100 => return .aam,
        0b11010101 => return .aad,
        0b10011000 => return .cbw,
        0b10011001 => return .cwd,

        0b00101000...0b00101011, // sub reg/mem from reg to reg/mem
        0b00101100...0b00101101, // sub imm to ax
        => return .sub,

        0b00011000...0b00011011, // sbb reg/mem from reg to reg/mem
        0b00011100...0b00011101, // sbb imm to ax
        => return .sbb,

        0b00111000...0b00111011, // cmp reg/mem and reg
        0b00111100...0b00111101, // cmp imm with ax
        => return .cmp,

        0b01001000...0b01001111, // dec register
        => return .dec,

        0b00100000...0b00100011, // and reg/mem and reg
        0b00100100...0b00100101, // and imm with ax
        => return .@"and",

        0b00001000...0b00001011, // or reg/mem with reg
        0b00001100...0b00001101, // or imm with ax
        => return .@"or",

        0b00110000...0b00110011, // xor reg/mem with reg
        0b00110100...0b00110101, // xor imm with ax
        => return .xor,

        0b10000100...0b10000101, // test
        0b10101000...0b10101001, // test with ax
        => return .@"test",

        0b11000011 => return .ret, // within segment
        0b11000010 => return .ret, // within segment
        0b11001011 => return .retf, // inter segment
        0b11001010 => return .retf, // inter segment
        0b10011010 => return .call, // inter segment
        0b11101010 => return .jmp, // inter segment
        0b11101001 => return .jmp, // jmp direct within segment
        0b11101000 => return .call, // call direct within segment
        0b11101011 => return .jmp, // jmp direct withing segment - short
        0b01110100 => return .je,
        0b01111100 => return .jl,
        0b01111110 => return .jle,
        0b01110010 => return .jb,
        0b01110110 => return .jbe,
        0b01111010 => return .jp,
        0b01110000 => return .jo,
        0b01111000 => return .js,
        0b01110101 => return .jne,
        0b01111101 => return .jnl,
        0b01111111 => return .jg,
        0b01110011 => return .jnb,
        0b01110111 => return .jnbe,
        0b01111011 => return .jnp,
        0b01110001 => return .jno,
        0b01111001 => return .jns,
        0b11100010 => return .loop,
        0b11100001 => return .loopz,
        0b11100000 => return .loopnz,
        0b11100011 => return .jcxz,

        0b01010000...0b01010111,
        0b00000110,
        0b00001110,
        0b00010110,
        0b00011110,
        0b00100110,
        0b00101110,
        0b00110110,
        0b00111110,
        => return .push,

        0b10001111,
        0b01011000...0b01011111,
        0b00000111,
        0b00001111,
        0b00010111,
        0b00011111,
        => return .pop,

        0b10011100 => return .pushf,
        0b10011101 => return .popf,

        0b10000110...0b10000111,
        0b10010000...0b10010111,
        => return .xchg,

        0b11100100...0b11100101,
        0b11101100...0b11101101,
        => return .in,

        0b11100110...0b11100111,
        0b11101110...0b11101111,
        => return .out,

        0b11010111 => return .xlat,
        0b10001101 => return .lea,
        0b11000101 => return .lds,
        0b11000100 => return .les,
        0b10011111 => return .lahf,
        0b10011110 => return .sahf,

        0b1010_010_0 => .movsb,
        0b1010_011_0 => .cmpsb,
        0b1010_111_0 => .scasb,
        0b1010_110_0 => .lodsb,
        0b1010_101_0 => .stosb,
        0b1010_010_1 => .movsw,
        0b1010_011_1 => .cmpsw,
        0b1010_111_1 => .scasw,
        0b1010_110_1 => .lodsw,
        0b1010_101_1 => .stosw,

        0b11001101 => .int,
        0b11001100 => .int3,
        0b11001110 => .into,
        0b11001111 => .iret,
        0b11111000 => .clc,
        0b11110101 => .cmc,
        0b11111001 => .stc,
        0b11111100 => .cld,
        0b11111101 => .std,
        0b11111010 => .cli,
        0b11111011 => .sti,
        0b11110100 => .hlt,
        0b10011011 => .wait,

        else => return .unknown,
    };
}

fn bitsToMnemonic2(bytes: [2]u8) nms.Mnemonic {
    const first_try = bitsToMnemonic(bytes[0]);
    if (first_try != .unknown) return first_try;

    return switch (bytes[0]) {
        // add, sub, cmp, etc imm from reg/mem
        0b10000000...0b10000011 => switch (bytes[1] & 0b00111000) {
            0b00000000 => .add,
            0b00010000 => .adc,
            0b00101000 => .sub,
            0b00011000 => .sbb,
            0b00111000 => .cmp,
            0b00100000 => .@"and",
            0b00001000 => .@"or",
            0b00110000 => .xor,
            else => .unknown,
        },

        // mul, imul, div, idiv, neg, not, test
        0b11110110...0b11110111 => switch (bytes[1] & 0b00111000) {
            0b00100000 => .mul,
            0b00101000 => .imul,
            0b00110000 => .div,
            0b00111000 => .idiv,
            0b00011000 => .neg,
            0b00010000 => .not,
            0b00000000 => .@"test",
            else => .unknown,
        },

        // shifts and rotates
        0b11010000...0b11010011 => switch (bytes[1] & 0b00111000) {
            0b00100000 => .shl,
            0b00101000 => .shr,
            0b00111000 => .sar,
            0b00000000 => .rol,
            0b00001000 => .ror,
            0b00010000 => .rcl,
            0b00011000 => .rcr,
            else => .unknown,
        },

        // push, inc, dec reg/mem
        // call, indirect jump
        0b11111110...0b11111111 => switch (bytes[1] & 0b00111000) {
            0b00000000 => .inc,
            0b00001000 => .dec,
            0b00110000 => .push,
            0b00010000 => .call,
            0b00011000 => .@"call far",
            0b00100000 => .jmp,
            0b00101000 => .@"jmp far",
            else => .unknown,
        },

        else => .unknown,
    };
}

const ConstantPrefix = enum {
    not,
    byte,
    word,
    rep,
    lock,
};

const SegmentPrefix = enum {
    cs,
    ds,
    es,
    ss,
};

const Prefix = union(enum) {
    none: void,
    cnst: ConstantPrefix,
    seg: SegmentPrefix,
    imm: i16,

    // The size of the return value should match the size of the prefix fields in
    // Instruction and SrcDst.
    pub fn init(comptime num: u8, inits: [num]Prefix) [2]Prefix {
        var result = Prefix.init0();
        if (inits.len > 0) result[0] = inits[0];
        if (inits.len > 1) result[1] = inits[1];
        return result;
    }

    pub fn init1C(c: ConstantPrefix) [2]Prefix {
        return Prefix.init(1, .{.{ .cnst = c }});
    }

    pub fn init0() [2]Prefix {
        return [_]Prefix{ .{ .none = {} }, .{ .none = {} } };
    }
};

/// Source or destination for an instruction.
pub const SrcDst = struct {
    op0: ?Operand,
    op1: ?Operand,
    op2: ?Operand,
    prefixes: [2]Prefix = .{ .none, .none },

    pub fn init(comptime num: u8, inits: [num]Operand, prefixes: [2]Prefix) SrcDst {
        var result = SrcDst{
            .op0 = null,
            .op1 = null,
            .op2 = null,
            .prefixes = prefixes,
        };
        if (inits.len > 0) result.op0 = inits[0];
        if (inits.len > 1) result.op1 = inits[1];
        if (inits.len > 2) result.op2 = inits[2];
        return result;
    }

    pub fn numOps(self: SrcDst) usize {
        var i: usize = 0;
        i += @boolToInt(self.op0 != null);
        i += @boolToInt(self.op1 != null);
        i += @boolToInt(self.op2 != null);
        return i;
    }
};

const InstructionParts = struct {
    mod_rm: SrcDst,
    reg: SrcDst,
    immediate: ?SrcDst,
    encoded_bytes: u8, // never more than 6

    /// Many instructions share a common encoding scheme:
    ///
    /// [ byte0 ] [ mod/reg/rm ] [ disp lo ] [ disp hi ] [ data lo ] [ data hi ]
    /// disp and data are optional.
    ///
    /// This function reads between 2 and 6 bytes and decodes the mod/reg/rm, disp, and
    /// immediate
    pub fn init(
        bytes: []const u8,
        wide: bool,
        has_immediate: bool,
        immediate_is_wide: bool,
        prefixes: *[2]Prefix,
    ) InstructionParts {
        std.debug.assert(bytes.len >= 2);
        const mod_rm_result = getModRm(bytes, wide, prefixes);
        const mod_rm = mod_rm_result.srcdst;
        const encoded_displacement_size = mod_rm_result.encoded_displacement_size;

        const immediate = if (has_immediate)
            getImmediate(bytes, encoded_displacement_size, immediate_is_wide)
        else
            null;
        const encoded_immediate_size = if (has_immediate)
            @as(u8, 1) + @boolToInt(immediate_is_wide)
        else
            0;

        const reg = SrcDst.init(
            1,
            .{.{ .reg = bitsToReg(@truncate(u3, bytes[1] >> 3), @boolToInt(wide)) }},
            Prefix.init0(),
        );

        const encoded_bytes = 2 + encoded_displacement_size + encoded_immediate_size;
        std.debug.assert(encoded_bytes <= 6);

        return InstructionParts{
            .mod_rm = mod_rm,
            .reg = reg,
            .immediate = immediate,
            .encoded_bytes = encoded_bytes,
        };
    }

    const ModRmResult = struct {
        srcdst: SrcDst,
        encoded_displacement_size: u8,
    };

    fn getModRm(bytes: []const u8, wide: bool, prefixes: *[2]Prefix) ModRmResult {
        var result = getModRmFromBits(bytes, wide);
        // apply any segment registers in prefixes
        for (prefixes) |p| {
            switch (p) {
                .seg => {
                    var blank: usize = blk: {
                        for (result.srcdst.prefixes, 0..) |rp, i| {
                            switch (rp) {
                                .none => break :blk i,
                                else => {},
                            }
                        }
                        unreachable;
                    };
                    result.srcdst.prefixes[blank] = p;
                },
                else => {},
            }
        }

        // TODO: originally I was going to also remove the segment prefixes from the input
        // list too, but i went with the lazy solution of just ignoring them at print time
        // for instruction mnemonics.

        return result;
    }

    fn getModRmFromBits(bytes: []const u8, wide: bool) ModRmResult {
        const mod = @truncate(u2, bytes[1] >> 6);
        const rm = @truncate(u3, bytes[1]);

        if (mod == 0b11) {
            // register with no displacement
            return .{
                .srcdst = SrcDst.init(1, .{.{ .reg = bitsToReg(rm, @boolToInt(wide)) }}, Prefix.init0()),
                .encoded_displacement_size = 0,
            };
        }

        // there is some effective address calculation present

        // how big is the [displo]/[disphi] encoded portion?
        const encoded_displacement_size: u8 = blk: {
            if ((mod == 0b00 and rm != 0b110) or mod == 0b11) {
                // instruction is two bytes long w/ no encoded (i.e. an implicit) displacement
                break :blk 0;
            } else if (mod == 0b01) {
                // byte displacement. these are treated as signed integers and
                // are sign extended (handled implicitly) to an i16 for computation.
                break :blk 1;
            } else if (mod == 0b10 or (mod == 0b00 and rm == 0b110)) {
                // word displacement or the special 0b11 case:
                // a direct address mov with a two byte operand
                break :blk 2;
            } else {
                unreachable;
            }
        };

        // now decode the displacement
        const displacement = blk: {
            switch (encoded_displacement_size) {
                0 => {
                    // will go unused
                    break :blk undefined;
                },
                1 => {
                    const byte2 = bytes[2];
                    break :blk Operand{ .imm = .{ .imm16 = @bitCast(i8, byte2) } };
                },
                2 => {
                    const byte2 = bytes[2];
                    const byte3 = bytes[3];
                    break :blk Operand{ .imm = .{ .imm16 = (@as(i16, byte3) << 8) | byte2 } };
                },
                else => unreachable,
            }
        };

        return .{
            .srcdst = bitsToSrcDst(rm, mod, @boolToInt(wide), displacement),
            .encoded_displacement_size = encoded_displacement_size,
        };
    }

    fn getImmediate(bytes: []const u8, encoded_displacement_size: u8, immediate_is_wide: bool) SrcDst {
        var byte_low: u8 = bytes[2 + encoded_displacement_size];
        var byte_high: u8 = if (immediate_is_wide) bytes[2 + encoded_displacement_size + 1] else 0;
        const imm_op = Operand{ .imm = .{ .imm16 = byte_low | (@as(i16, byte_high) << 8) } };
        return SrcDst.init(
            1,
            .{imm_op},
            if (immediate_is_wide) Prefix.init1C(.word) else Prefix.init1C(.byte),
        );
    }
};

/// The main instruction struct. Each instruction is decoded into an instance of this.
pub const Instruction = struct {
    prefixes: [2]Prefix,
    mnemonic: nms.Mnemonic,
    /// Destination operand(s). Some instructions only make use of 1 or 2 operands, in
    /// which case the decoder will fill the buffer with `Operand.none`.
    destination: SrcDst,
    /// Source operands(s). Uses the same `none` packing logic as `destination`.
    source: SrcDst,
    /// Size of the operand in machine code.
    encoded_bytes: u8,
    binary_index: usize,
};

/// Iterate through a byte buffer and produce `Instruction`s.
pub const DecodeIterator = struct {
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
        var prefixes = Prefix.init0();
        var prefixes_len: usize = 0;
        return self.nextWPrefix(&prefixes, prefixes_len);
    }

    fn nextWPrefix(
        self: *DecodeIterator,
        prefixes: *[2]Prefix,
        prefixes_len: usize,
    ) ?Instruction {
        if (self.index >= self.bytes.len) return null;

        switch (self.bytes[self.index]) {
            0b10001000...0b10001011, // mov reg/mem to/from reg,
            0b00000000...0b00000011, // add reg/mem with reg to reg/mem,
            0b00010000...0b00010011, // adc reg/mem with reg to reg/mem,
            0b00101000...0b00101011, // sub reg/mem from reg to reg/mem,
            0b00011000...0b00011011, // sbb reg/mem from reg to reg/mem,
            0b00111000...0b00111011, // cmp reg/mem with reg,
            0b00100000...0b00100011, // and reg/mem with reg to reg/mem,
            0b10000110...0b10000111, // xchg reg/mem with reg
            0b00001000...0b00001011, // or reg/mem with reg
            0b00110000...0b00110011, // xor reg/mem with reg
            => {
                const end = @min(self.bytes.len, self.index + 6);
                const slice = self.bytes[self.index..end];

                const wide = @truncate(u1, slice[0]) == 1;
                const has_immediate = false;
                const immediate_is_wide = false;
                const d_bit = @truncate(u1, slice[0] >> 1) == 1;

                const ip = InstructionParts.init(slice, wide, has_immediate, immediate_is_wide, prefixes);
                defer self.index += ip.encoded_bytes;

                var i = Instruction{
                    .prefixes = prefixes.*,
                    .mnemonic = bitsToMnemonic2(slice[0..2].*),
                    .destination = if (d_bit) ip.reg else ip.mod_rm,
                    .source = if (d_bit) ip.mod_rm else ip.reg,
                    .encoded_bytes = ip.encoded_bytes,
                    .binary_index = self.index,
                };

                return i;
            },

            0b10001110, // reg/mem to segreg
            0b10001100, // segreg to reg/mem
            => {
                const end = @min(self.bytes.len, self.index + 6);
                const slice = self.bytes[self.index..end];

                const wide = true;
                const has_immediate = false;
                const immediate_is_wide = false;
                const d_bit = @truncate(u1, slice[0] >> 1) == 1;

                var ip = InstructionParts.init(slice, wide, has_immediate, immediate_is_wide, prefixes);
                defer self.index += ip.encoded_bytes;

                // replace parsed register with seg reg
                ip.reg = SrcDst.init(
                    1,
                    .{.{ .reg = bitsToSegReg(@truncate(u2, self.bytes[self.index + 1] >> 3)) }},
                    ip.reg.prefixes,
                );

                var i = Instruction{
                    .prefixes = prefixes.*,
                    .mnemonic = bitsToMnemonic2(slice[0..2].*),
                    .destination = if (d_bit) ip.reg else ip.mod_rm,
                    .source = if (d_bit) ip.mod_rm else ip.reg,
                    .encoded_bytes = ip.encoded_bytes,
                    .binary_index = self.index,
                };

                return i;
            },

            0b10000100...0b10000101, // test reg/mem with reg,
            => {
                const end = @min(self.bytes.len, self.index + 6);
                const slice = self.bytes[self.index..end];

                const wide = @truncate(u1, slice[0]) == 1;
                const has_immediate = false;
                const immediate_is_wide = false;

                const ip = InstructionParts.init(slice, wide, has_immediate, immediate_is_wide, prefixes);
                defer self.index += ip.encoded_bytes;

                var i = Instruction{
                    .prefixes = prefixes.*,
                    .mnemonic = bitsToMnemonic2(slice[0..2].*),
                    .destination = ip.mod_rm,
                    .source = ip.reg,
                    .encoded_bytes = ip.encoded_bytes,
                    .binary_index = self.index,
                };

                return i;
            },

            0b10001101, // lea
            0b11000101, // lds
            0b11000100, // les
            => {
                const end = @min(self.bytes.len, self.index + 6);
                const slice = self.bytes[self.index..end];

                // these loads are implicitly wide
                const wide = true;
                const has_immediate = false;
                const immediate_is_wide = false;

                var ip = InstructionParts.init(slice, wide, has_immediate, immediate_is_wide, prefixes);
                defer self.index += ip.encoded_bytes;

                // nasm doesn't like having explicit sizes on these instructions
                ip.mod_rm.prefixes = Prefix.init0();

                var i = Instruction{
                    .prefixes = prefixes.*,
                    .mnemonic = bitsToMnemonic2(slice[0..2].*),
                    .destination = ip.reg,
                    .source = ip.mod_rm,
                    .encoded_bytes = ip.encoded_bytes,
                    .binary_index = self.index,
                };

                return i;
            },

            0b11000110...0b11000111, // mov imm to reg/mem,
            // arith* imm to reg/mem (*add, adc, sub, sbb, cmp, and, or, xor)
            0b10000000...0b10000011,
            => {
                const end = @min(self.bytes.len, self.index + 6);
                const slice = self.bytes[self.index..end];

                const wide = @truncate(u1, slice[0]) == 1;

                const has_immediate = true;
                const immediate_is_wide = blk: {
                    if (slice[0] & 0b11111110 == 0b11000110) {
                        // in a mov imm, the w bit alone determines the size of the immediate
                        break :blk wide;
                    } else if (slice[0] & 0b11111100 == 0b10000000) {
                        // in an arithmetic-like instruction, there is an extra s bit. the
                        // immediate is wide if s == 0 and w == 1.
                        const s = @truncate(u1, slice[0] >> 1) == 1;
                        break :blk !s and wide;
                    } else {
                        unreachable;
                    }
                };

                var ip = InstructionParts.init(slice, wide, has_immediate, immediate_is_wide, prefixes);
                defer self.index += ip.encoded_bytes;

                var i = Instruction{
                    .prefixes = prefixes.*,
                    .mnemonic = bitsToMnemonic2(slice[0..2].*),
                    .destination = ip.mod_rm,
                    .source = ip.immediate.?,
                    .encoded_bytes = ip.encoded_bytes,
                    .binary_index = self.index,
                };

                return i;
            },

            // * imm with ax
            0b00000100...0b00000101, // *add
            0b00010100...0b00010101, // *adc
            0b00101100...0b00101101, // *sub
            0b00011100...0b00011101, // *sbb
            0b00111100...0b00111101, // *cmp
            0b00100100...0b00100101, // *and
            0b10101000...0b10101001, // *test
            0b00001100...0b00001101, // *or
            0b00110100...0b00110101, // *xor
            => {
                const w = @truncate(u1, self.bytes[self.index]);
                const byte1 = self.bytes[self.index + 1];
                const byte2 = if (w == 1) self.bytes[self.index + 2] else undefined;

                const imm = if (w == 0)
                    Operand{ .imm = .{ .imm16 = @bitCast(i8, byte1) } }
                else
                    Operand{ .imm = .{ .imm16 = @as(i16, byte2) << 8 | byte1 } };

                const reg = if (w == 0)
                    Operand{ .reg = .al }
                else
                    Operand{ .reg = .ax };

                defer self.index += @as(u8, 2) + w;

                const size = Prefix{ .cnst = if (w == 0) .byte else .word };
                const pre = Prefix.init(1, .{size});

                return Instruction{
                    .prefixes = prefixes.*,
                    .mnemonic = bitsToMnemonic(self.bytes[self.index]),
                    .destination = SrcDst.init(1, .{reg}, pre),
                    .source = SrcDst.init(1, .{imm}, pre),
                    .encoded_bytes = @as(u8, 2) + w,
                    .binary_index = self.index,
                };
            },

            0b11111110...0b11111111, // push/inc/dec reg/mem
            0b10001111, // pop reg/mem
            0b11110110...0b11110111, // mul/imul/div/idiv/neg/not/test
            => {
                const end = @min(self.bytes.len, self.index + 6);
                const slice = self.bytes[self.index..end];

                const wide = @truncate(u1, self.bytes[self.index]) == 1;
                // test is unique in that it always has an immediate
                const has_immediate = slice[0] & 0b11111110 == 0b11110110 and slice[1] & 0b00111000 == 0b00000000;
                const immediate_is_wide = wide;

                const ip = InstructionParts.init(slice, wide, has_immediate, immediate_is_wide, prefixes);
                defer self.index += ip.encoded_bytes;

                var i = Instruction{
                    .prefixes = prefixes.*,
                    .mnemonic = bitsToMnemonic2(slice[0..2].*),
                    .destination = ip.mod_rm,
                    .source = if (has_immediate) ip.immediate.? else SrcDst.init(0, .{}, undefined), // TODO consider making this null
                    .encoded_bytes = ip.encoded_bytes,
                    .binary_index = self.index,
                };

                return i;
            },

            // push/pop/inc/dec register oooooreg
            0b01010000...0b01011111, // push/pop
            0b01000000...0b01000111, // inc
            0b01001000...0b01001111, // dec
            => {
                // w is implicitly 1
                const reg = bitsToReg(@truncate(u3, self.bytes[self.index]), 1);

                defer self.index += 1;
                return Instruction{
                    .prefixes = prefixes.*,
                    .mnemonic = bitsToMnemonic(self.bytes[self.index]),
                    .destination = SrcDst.init(1, .{.{ .reg = reg }}, Prefix.init1C(.byte)),
                    .source = SrcDst.init(0, .{}, Prefix.init1C(.byte)),
                    .encoded_bytes = 1,
                    .binary_index = self.index,
                };
            },

            // ret with sp offset
            0b11000010,
            0b11001010,
            => {
                const end = @min(self.bytes.len, self.index + 6);
                const slice = self.bytes[self.index..end];

                const imm = (@as(i16, slice[2]) << 8) | slice[1];
                const imm_srcdst = SrcDst.init(1, .{.{ .imm = .{ .imm16 = imm } }}, .{ .none, .none });

                defer self.index += 3;

                var i = Instruction{
                    .prefixes = prefixes.*,
                    .mnemonic = bitsToMnemonic2(slice[0..2].*),
                    .destination = SrcDst.init(0, .{}, undefined),
                    .source = imm_srcdst,
                    .encoded_bytes = 3,
                    .binary_index = self.index,
                };

                return i;
            },

            0b11010000...0b11010011, // shl/shr/sar/rol/ror/rcl/rcr
            => {
                const end = @min(self.bytes.len, self.index + 6);
                const slice = self.bytes[self.index..end];

                const wide = @truncate(u1, slice[0]) == 1;
                const has_immediate = false;
                const immediate_is_wide = false;

                const ip = InstructionParts.init(slice, wide, has_immediate, immediate_is_wide, prefixes);
                defer self.index += ip.encoded_bytes;

                // the 2nd lsb determines if the src is the immediate 1 or the register
                // cl. see table 4-7 and page 4-24
                const v_bit = @truncate(u1, slice[0] >> 1) == 1;

                const source = if (v_bit)
                    SrcDst.init(1, .{.{ .reg = .cl }}, Prefix.init0())
                else
                    SrcDst.init(1, .{.{ .imm = .{ .imm16 = 1 } }}, Prefix.init0());

                var i = Instruction{
                    .prefixes = prefixes.*,
                    .mnemonic = bitsToMnemonic2(slice[0..2].*),
                    .destination = ip.mod_rm,
                    .source = source,
                    .encoded_bytes = ip.encoded_bytes,
                    .binary_index = self.index,
                };

                return i;
            },

            // mov imm to reg
            // 1011wreg
            0b10110000...0b10111111 => {
                const w = @truncate(u1, self.bytes[self.index] >> 3);
                const reg_bits = @truncate(u3, self.bytes[self.index]);
                defer self.index += @as(u8, 2) + w;
                const byte1 = self.bytes[self.index + 1];
                const imm = blk: {
                    const byte2 = if (w == 1) self.bytes[self.index + 2] else undefined;
                    break :blk if (w == 0) Operand{ .imm = .{ .imm16 = @bitCast(i8, byte1) } } else Operand{ .imm = .{ .imm16 = (@as(i16, byte2) << 8) | byte1 } };
                };

                const reg = bitsToReg(reg_bits, w);
                const reg_op = Operand{ .reg = reg };
                const size = Prefix.init1C(if (w == 0) .byte else .word);

                return Instruction{
                    .prefixes = prefixes.*,
                    .mnemonic = .mov,
                    .destination = SrcDst.init(1, .{reg_op}, size),
                    .source = SrcDst.init(1, .{imm}, size),
                    .encoded_bytes = @as(u8, 2) + w,
                    .binary_index = self.index,
                };
            },

            // mov ax/mem to ax/mem
            // 101000dw
            0b10100000...0b10100011 => {
                // for some reason, the usual d logic is reversed for this instruction.
                // (it's not actually labeled as a d bit in the manual)
                const not_d = @truncate(u1, self.bytes[self.index] >> 1);
                const w = @truncate(u1, self.bytes[self.index]);

                defer self.index += @as(u8, 2) + w;

                const byte1 = self.bytes[self.index + 1];
                const byte2 = self.bytes[self.index + 2];

                const reg: nms.Register = if (w == 0) .al else .ax;
                const reg_ops = SrcDst.init(1, .{.{ .reg = reg }}, Prefix.init1C(.byte));

                const addr8 = byte1;
                const addr16 = (@as(u16, byte2) << 8) | addr8;

                const mem_ops = if (w == 0)
                    SrcDst.init(
                        2,
                        .{ .{ .imm = .{ .imm16 = @bitCast(i8, addr8) } }, .{ .imm = .{ .imm16 = 0 } } },
                        Prefix.init1C(.byte),
                    )
                else
                    SrcDst.init(
                        2,
                        .{ .{ .imm = .{ .imm16 = @bitCast(i16, addr16) } }, .{ .imm = .{ .imm16 = 0 } } },
                        Prefix.init1C(.word),
                    );

                var dst = &reg_ops;
                var src = &mem_ops;
                if (not_d == 1) std.mem.swap(*const SrcDst, &dst, &src);

                return Instruction{
                    .prefixes = prefixes.*,
                    .mnemonic = .mov,
                    .destination = dst.*,
                    .source = src.*,
                    .encoded_bytes = @as(u8, 2) + w,
                    .binary_index = self.index,
                };
            },

            // xchg reg with ax 0b10010reg
            0b10010000...0b10010111 => {
                defer self.index += 1;
                const reg = bitsToReg(@truncate(u3, self.bytes[self.index]), 1);
                return Instruction{
                    .prefixes = prefixes.*,
                    .mnemonic = .xchg,
                    .destination = SrcDst.init(1, .{.{ .reg = .ax }}, Prefix.init1C(.word)),
                    .source = SrcDst.init(1, .{.{ .reg = reg }}, Prefix.init1C(.word)),
                    .encoded_bytes = 1,
                    .binary_index = self.index,
                };
            },

            // various jumps
            0b01110000...0b01111111,
            0b11100010,
            0b11100001,
            0b11100000,
            0b11100011,
            => {
                defer self.index += 2;

                const byte1 = self.bytes[self.index + 1];

                const jump_amount = SrcDst.init(
                    1,
                    .{.{ .imm = .{ .inst_addr = @bitCast(i8, byte1) } }},
                    Prefix.init0(),
                );

                return Instruction{
                    .prefixes = prefixes.*,
                    .mnemonic = bitsToMnemonic(self.bytes[self.index]),
                    .destination = jump_amount,
                    .source = SrcDst.init(0, .{}, undefined),
                    .encoded_bytes = 2,
                    .binary_index = self.index,
                };
            },

            0b10011010, // direct intersegment call
            0b11101010, // direct intersegment jump
            => {
                defer self.index += 5;
                const ip = (@as(i16, self.bytes[self.index + 2]) << 8) | self.bytes[self.index + 1];
                const cs = (@as(i16, self.bytes[self.index + 4]) << 8) | self.bytes[self.index + 3];
                const ip_sd = SrcDst.init(1, .{.{ .imm = .{ .imm16 = ip } }}, .{ .{ .imm = cs }, .none });
                return Instruction{
                    .prefixes = prefixes.*,
                    .mnemonic = bitsToMnemonic(self.bytes[self.index]),
                    .destination = ip_sd,
                    .source = SrcDst.init(0, .{}, undefined),
                    .encoded_bytes = 5,
                    .binary_index = self.index,
                };
            },

            0b11101001, // jmp direct within segment
            0b11101000, // call direct within segment
            => {
                defer self.index += 3;
                var ip = (@as(i16, self.bytes[self.index + 2]) << 8) | self.bytes[self.index + 1];

                // within segment jmps/calls are relative
                ip += @truncate(i16, @bitCast(i64, self.index)) + 3;

                const ip_sd = SrcDst.init(1, .{.{ .imm = .{ .imm16 = ip } }}, .{ .none, .none });
                return Instruction{
                    .prefixes = prefixes.*,
                    .mnemonic = bitsToMnemonic(self.bytes[self.index]),
                    .destination = ip_sd,
                    .source = SrcDst.init(0, .{}, undefined),
                    .encoded_bytes = 3,
                    .binary_index = self.index,
                };
            },

            0b11101011, // jmp direct withing segment - short
            => {
                defer self.index += 2;
                const ip = @bitCast(i8, self.bytes[self.index + 1]);
                const ip_sd = SrcDst.init(1, .{.{ .imm = .{ .imm16 = ip } }}, .{ .none, .none });
                return Instruction{
                    .prefixes = prefixes.*,
                    .mnemonic = bitsToMnemonic(self.bytes[self.index]),
                    .destination = ip_sd,
                    .source = SrcDst.init(0, .{}, undefined),
                    .encoded_bytes = 1,
                    .binary_index = self.index,
                };
            },

            // int
            0b11001101 => {
                defer self.index += 2;
                const byte1 = self.bytes[self.index + 1];
                return Instruction{
                    .prefixes = prefixes.*,
                    .mnemonic = bitsToMnemonic(self.bytes[self.index]),
                    .destination = SrcDst.init(1, .{.{ .imm = .{ .imm16 = @as(i16, byte1) } }}, .{ .none, .none }),
                    .source = SrcDst.init(0, .{}, undefined),
                    .encoded_bytes = 2,
                    .binary_index = self.index,
                };
            },

            // in/out
            0b11100100...0b11100101,
            0b11101100...0b11101101,
            0b11100110...0b11100111,
            0b11101110...0b11101111,
            => {
                // for these specific opcodes, the 4th lsb happens to be 0 when an extra
                // byte of data is present.
                const has_data = (self.bytes[self.index] & 0b0001000) == 0;
                const w = @truncate(u1, self.bytes[self.index]);
                defer self.index += @as(u8, 1) + @boolToInt(has_data);

                const dest = if (w == 1)
                    SrcDst.init(1, .{.{ .reg = .ax }}, Prefix.init1C(.word))
                else
                    SrcDst.init(1, .{.{ .reg = .al }}, Prefix.init1C(.byte));

                const src = blk: {
                    if (has_data) {
                        const byte1 = @bitCast(i8, self.bytes[self.index + 1]);
                        break :blk SrcDst.init(1, .{.{ .imm = .{ .imm16 = byte1 } }}, Prefix.init1C(.byte));
                    } else {
                        break :blk SrcDst.init(1, .{.{ .reg = .dx }}, Prefix.init1C(.word));
                    }
                };

                const mn = bitsToMnemonic(self.bytes[self.index]);
                var src_ptr = &src;
                var dst_ptr = &dest;
                if (mn == .out) std.mem.swap(*const SrcDst, &src_ptr, &dst_ptr);

                return Instruction{
                    .prefixes = prefixes.*,
                    .mnemonic = mn,
                    .destination = dst_ptr.*,
                    .source = src_ptr.*,
                    .encoded_bytes = @as(u8, 1) + @boolToInt(has_data),
                    .binary_index = self.index,
                };
            },

            // push segment register
            0b00000110,
            0b00001110,
            0b00010110,
            0b00011110,
            // pop segment register
            0b00000111,
            0b00001111,
            0b00010111,
            0b00011111,
            => {
                defer self.index += 1;
                const reg = bitsToSegReg(@truncate(u2, self.bytes[self.index] >> 3));
                return Instruction{
                    .prefixes = prefixes.*,
                    .mnemonic = bitsToMnemonic(self.bytes[self.index]),
                    .destination = SrcDst.init(1, .{.{ .reg = reg }}, Prefix.init1C(.word)),
                    .source = SrcDst.init(0, .{}, undefined),
                    .encoded_bytes = 1,
                    .binary_index = self.index,
                };
            },

            // single byte no operand instructions
            0b10011100, // pushf
            0b10011101, // popf
            0b11010111, // xlat
            0b10011111, // lahf
            0b10011110, // sahf
            0b00110111, // aaa
            0b00100111, // daa
            0b00111111, // aas
            0b00101111, // das
            0b10011000, // cbw
            0b10011001, // cwd
            0b10100100, // movsb
            0b10100110, // cmpsb
            0b10101110, // scasb
            0b10101100, // lodsb
            0b10101010, // stosb
            0b10100101, // movsw
            0b10100111, // cmpsw
            0b10101111, // scasw
            0b10101101, // lodsw
            0b10101011, // stosw
            0b11000011, // ret (within segment)
            0b11001011, // ret (intersegment)
            0b11001100, // int3,
            0b11001110, // into,
            0b11001111, // iret,
            0b11111000, // clc,
            0b11110101, // cmc,
            0b11111001, // stc,
            0b11111100, // cld,
            0b11111101, // std,
            0b11111010, // cli,
            0b11111011, // sti,
            0b11110100, // hlt,
            0b10011011, // wait,
            => {
                defer self.index += 1;
                return Instruction{
                    .prefixes = prefixes.*,
                    .mnemonic = bitsToMnemonic(self.bytes[self.index]),
                    .destination = SrcDst.init(0, .{}, undefined),
                    .source = SrcDst.init(0, .{}, undefined),
                    .encoded_bytes = 1,
                    .binary_index = self.index,
                };
            },

            // double byte no operand instructions
            0b11010100, // aam
            0b11010101, // aad
            => {
                defer self.index += 2;
                return Instruction{
                    .prefixes = prefixes.*,
                    .mnemonic = bitsToMnemonic(self.bytes[self.index]),
                    .destination = SrcDst.init(0, .{}, undefined),
                    .source = SrcDst.init(0, .{}, undefined),
                    .encoded_bytes = 2,
                    .binary_index = self.index,
                };
            },

            // prefixes
            0b11110010...0b11110011, // rep
            0b11110000, // lock
            0b00100110, // es
            0b00101110, // cs
            0b00110110, // ss
            0b00111110, // ds
            => {
                var blank: usize = blk: {
                    for (prefixes, 0..) |p, i| {
                        switch (p) {
                            .none => break :blk i,
                            else => {},
                        }
                    }
                    // no room!
                    std.debug.print("ERROR: no room for another instruction prefix!\n", .{});
                    unreachable;
                };

                prefixes[blank] = bitsToPrefix(self.bytes[self.index]);

                self.index += 1;
                var prefixed = self.nextWPrefix(prefixes, prefixes_len + 1);
                if (prefixed) |*pf| {
                    pf.encoded_bytes += 1;
                    pf.binary_index -= 1;
                }
                return prefixed;
            },

            else => {
                defer self.index += 1;
                return Instruction{
                    .prefixes = prefixes.*,
                    .mnemonic = .unknown,
                    .destination = SrcDst.init(0, .{}, undefined),
                    .source = SrcDst.init(0, .{}, undefined),
                    .encoded_bytes = 1,
                    .binary_index = self.index,
                };
            },
        }
    }
};

fn bitsToSrcDst(reg_or_mem: u3, mod: u2, w: u1, imm_value: Operand) SrcDst {
    // see table 4-10. we combine the bits into a 5 bit number, taking mod to be the most
    // significant bits
    var i: u5 = mod;
    i <<= 3;
    i |= reg_or_mem;

    const size = Prefix.init1C(if (w == 0) .byte else .word);

    // TODO: tame this beast
    return switch (i) {
        // mod == 0b00
        0b00_000 => SrcDst.init(2, .{ .{ .reg = .bx }, .{ .reg = .si } }, size),
        0b00_001 => SrcDst.init(2, .{ .{ .reg = .bx }, .{ .reg = .di } }, size),
        0b00_010 => SrcDst.init(2, .{ .{ .reg = .bp }, .{ .reg = .si } }, size),
        0b00_011 => SrcDst.init(2, .{ .{ .reg = .bp }, .{ .reg = .di } }, size),
        // these have an immediate zero in them so the printer prints them as effective
        // address calculations.
        0b00_100 => SrcDst.init(2, .{ .{ .reg = .si }, .{ .imm = .{ .imm16 = 0 } } }, size),
        0b00_101 => SrcDst.init(2, .{ .{ .reg = .di }, .{ .imm = .{ .imm16 = 0 } } }, size),
        0b00_110 => SrcDst.init(2, .{ imm_value, .{ .imm = .{ .imm16 = 0 } } }, size),
        0b00_111 => SrcDst.init(2, .{ .{ .reg = .bx }, .{ .imm = .{ .imm16 = 0 } } }, size),

        // mod == 0b01
        0b01_000 => SrcDst.init(3, .{ .{ .reg = .bx }, .{ .reg = .si }, imm_value }, size),
        0b01_001 => SrcDst.init(3, .{ .{ .reg = .bx }, .{ .reg = .di }, imm_value }, size),
        0b01_010 => SrcDst.init(3, .{ .{ .reg = .bp }, .{ .reg = .si }, imm_value }, size),
        0b01_011 => SrcDst.init(3, .{ .{ .reg = .bp }, .{ .reg = .di }, imm_value }, size),
        0b01_100 => SrcDst.init(2, .{ .{ .reg = .si }, imm_value }, size),
        0b01_101 => SrcDst.init(2, .{ .{ .reg = .di }, imm_value }, size),
        0b01_110 => SrcDst.init(2, .{ .{ .reg = .bp }, imm_value }, size),
        0b01_111 => SrcDst.init(2, .{ .{ .reg = .bx }, imm_value }, size),

        // mod == 0b10
        0b10_000 => SrcDst.init(3, .{ .{ .reg = .bx }, .{ .reg = .si }, imm_value }, size),
        0b10_001 => SrcDst.init(3, .{ .{ .reg = .bx }, .{ .reg = .di }, imm_value }, size),
        0b10_010 => SrcDst.init(3, .{ .{ .reg = .bp }, .{ .reg = .si }, imm_value }, size),
        0b10_011 => SrcDst.init(3, .{ .{ .reg = .bp }, .{ .reg = .di }, imm_value }, size),
        0b10_100 => SrcDst.init(2, .{ .{ .reg = .si }, imm_value }, size),
        0b10_101 => SrcDst.init(2, .{ .{ .reg = .di }, imm_value }, size),
        0b10_110 => SrcDst.init(2, .{ .{ .reg = .bp }, imm_value }, size),
        0b10_111 => SrcDst.init(2, .{ .{ .reg = .bx }, imm_value }, size),

        // mod == 0b11. this case doesn't have any displacements so we just use bitsToReg
        0b11_000...0b11_111 => SrcDst.init(1, .{.{ .reg = bitsToReg(reg_or_mem, w) }}, size),
    };
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

    var labels = std.AutoHashMap(usize, usize).init(alctr);
    defer labels.deinit();
    var next_label_num: usize = 1;

    // first pass: derive labels for jump instructions
    for (instructions.items) |inst| {
        if (inst.destination.numOps() > 0 and std.meta.activeTag(inst.destination.op0.?) == .imm and std.meta.activeTag(inst.destination.op0.?.imm) == .inst_addr) {
            const rel_addr = inst.destination.op0.?.imm.inst_addr;
            const abs_addr = @intCast(usize, @intCast(i64, inst.binary_index) + rel_addr + 2);
            if (!labels.contains(abs_addr)) {
                try labels.put(abs_addr, next_label_num);
                next_label_num += 1;
            }
        }
    }

    // second pass: do the actual printing
    for (instructions.items) |inst| {
        // binary comment
        try writer.print("; 0x{X}", .{inst.binary_index});
        for (0..inst.encoded_bytes) |i| {
            try writer.print(" {b:0>8}", .{asm_bin[inst.binary_index + i]});
        }
        try writer.print("\n", .{});

        // label, if applicable
        if (labels.contains(inst.binary_index)) {
            try writer.print("L{?}:\n", .{labels.get(inst.binary_index)});
        }

        // instruction
        try txt.writeInst(inst, labels, writer);
        try writer.print("\n\n", .{});
    }
}

test "bitsToReg" {
    // from table 4-9 in intel 8086 users manual

    try expectEq(nms.Register.al, bitsToReg(0b000, 0));
    try expectEq(nms.Register.cl, bitsToReg(0b001, 0));
    try expectEq(nms.Register.dl, bitsToReg(0b010, 0));
    try expectEq(nms.Register.bl, bitsToReg(0b011, 0));
    try expectEq(nms.Register.ah, bitsToReg(0b100, 0));
    try expectEq(nms.Register.ch, bitsToReg(0b101, 0));
    try expectEq(nms.Register.dh, bitsToReg(0b110, 0));
    try expectEq(nms.Register.bh, bitsToReg(0b111, 0));

    try expectEq(nms.Register.ax, bitsToReg(0b000, 1));
    try expectEq(nms.Register.cx, bitsToReg(0b001, 1));
    try expectEq(nms.Register.dx, bitsToReg(0b010, 1));
    try expectEq(nms.Register.bx, bitsToReg(0b011, 1));
    try expectEq(nms.Register.sp, bitsToReg(0b100, 1));
    try expectEq(nms.Register.bp, bitsToReg(0b101, 1));
    try expectEq(nms.Register.si, bitsToReg(0b110, 1));
    try expectEq(nms.Register.di, bitsToReg(0b111, 1));
}

test "e2e 0037" {
    const alctr = std.testing.allocator;
    try tst.decodeEndToEnd("listing_0037_single_register_mov", alctr);
}

test "e2e 0038" {
    const alctr = std.testing.allocator;
    try tst.decodeEndToEnd("listing_0038_many_register_mov", alctr);
}

test "e2e 0039" {
    const alctr = std.testing.allocator;
    try tst.decodeEndToEnd("listing_0039_more_movs", alctr);
}

test "e2e 0040" {
    const alctr = std.testing.allocator;
    try tst.decodeEndToEnd("listing_0040_challenge_movs", alctr);
}

test "e2e 0040 positive" {
    const alctr = std.testing.allocator;
    try tst.decodeEndToEnd("listing_0040_challenge_movs_positive", alctr);
}

test "e2e negative displacement" {
    const alctr = std.testing.allocator;
    try tst.decodeEndToEnd("negative_mov", alctr);
}

test "e2e 0041" {
    const alctr = std.testing.allocator;
    try tst.decodeEndToEnd("listing_0041_add_sub_cmp_jnz", alctr);
}

test "e2e push pop" {
    const alctr = std.testing.allocator;
    try tst.decodeEndToEnd("push_pop", alctr);
}

test "e2e xchg" {
    const alctr = std.testing.allocator;
    try tst.decodeEndToEnd("xchg", alctr);
}

test "e2e in out" {
    const alctr = std.testing.allocator;
    try tst.decodeEndToEnd("in_out", alctr);
}

test "e2e loads" {
    const alctr = std.testing.allocator;
    try tst.decodeEndToEnd("loads", alctr);
}

test "e2e adds" {
    const alctr = std.testing.allocator;
    try tst.decodeEndToEnd("adds", alctr);
}

test "e2e subs" {
    const alctr = std.testing.allocator;
    try tst.decodeEndToEnd("subs", alctr);
}

test "e2e muls" {
    const alctr = std.testing.allocator;
    try tst.decodeEndToEnd("muls", alctr);
}

test "e2e logic_and_bits" {
    const alctr = std.testing.allocator;
    try tst.decodeEndToEnd("logic_and_bits", alctr);
}

test "e2e string" {
    const alctr = std.testing.allocator;
    try tst.decodeEndToEnd("string", alctr);
}

test "e2e jumps" {
    const alctr = std.testing.allocator;
    try tst.decodeEndToEnd("jumps", alctr);
}

test "e2e misc" {
    const alctr = std.testing.allocator;
    try tst.decodeEndToEnd("misc", alctr);
}

test "e2e prefixes" {
    const alctr = std.testing.allocator;
    try tst.decodeEndToEnd("prefixes", alctr);
}

test "e2e evil" {
    const alctr = std.testing.allocator;
    try tst.decodeEndToEnd("evil", alctr);
}

test "e2e 0042" {
    const alctr = std.testing.allocator;
    try tst.decodeEndToEnd("listing_0042_completionist_decode", alctr);
}

test "e2e 0043" {
    const alctr = std.testing.allocator;
    try tst.decodeEndToEnd("listing_0043_immediate_movs", alctr);
}

test "e2e 0044" {
    const alctr = std.testing.allocator;
    try tst.decodeEndToEnd("listing_0044_register_movs", alctr);
}

test "e2e 0045" {
    const alctr = std.testing.allocator;
    try tst.decodeEndToEnd("listing_0045_challenge_register_movs", alctr);
}

const std = @import("std");
const nms = @import("names.zig");
const txt = @import("text.zig");
const tst = @import("test.zig");

const expectEq = std.testing.expectEqual;
const expect = std.testing.expect;
