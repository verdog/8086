//! decoding machine language into assembly

/// 8086 register names. the order of the values in this enum has been chosen to match
/// table 4-9 in the 8086 users manual (page 263). the reg field is combined with the w
/// bit to make a number that maps to an enum value. see bitsToReg.
const Register = enum(u8) {
    // zig fmt: off
    al, cl, dl, bl, // a, b, c, d low. (byte)
    ah, ch, dh, bh, // a, b, c, d high. (byte)
    ax, cx, dx, bx, // a, b, c, d wide. (word, aka two bytes)
    sp, bp, si, di, // stack pointer, base pointer, source and destination. (word)
    es, cs, ss, ds, // segment registers
    // zig fmt: on
};

/// Instruction encoded immediate value.
const Immediate = union(enum) {
    inst_addr: i8, // instruction address. displayed differently (as labels).
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

    add,
    adc,
    inc,

    sub,
    sbb,
    dec,
    neg,

    cmp,

    aaa,
    daa,

    je,
    jl,
    jle,
    jb,
    jbe,
    jp,
    jo,
    js,
    jne,
    jnl,
    jg,
    jnb,
    jnbe,
    jnp,
    jno,
    jns,
    loop,
    loopz,
    loopnz,
    jcxz,

    push,
    pop,
    pushf,
    popf,

    xchg,

    in,
    out,

    xlat,

    lea,
    lds,
    les,
    lahf,
    sahf,

    unknown,

    pub fn init(byte: u8) Mnemonic {
        // see table 4-12
        return switch (byte) {
            0b10001000...0b10001011, // mov reg/mem to/from reg
            0b11000110...0b11000111, // mov imm to reg/mem
            0b10110000...0b10111111, // mov imm to reg
            0b10100000...0b10100011, // mov ax/mem to ax/mem
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

            0b11110110...0b11110111, // neg reg/mem
            => return .neg,

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

            else => return .unknown,
        };
    }

    pub fn init2(bytes: [2]u8) Mnemonic {
        const first_try = Mnemonic.init(bytes[0]);
        if (first_try != .unknown) return first_try;

        return switch (bytes[0]) {
            // add, sub, cmp, etc imm from reg/mem
            0b10000000...0b10000011 => switch (bytes[1] & 0b00111000) {
                0b00000000 => .add,
                0b00010000 => .adc,
                0b00101000 => .sub,
                0b00011000 => .sbb,
                0b00111000 => .cmp,
                else => .unknown,
            },

            // push, inc, dec reg/mem
            0b11111110...0b11111111 => switch (bytes[1] & 0b00111000) {
                0b00000000 => .inc,
                0b00001000 => .dec,
                0b00110000 => .push,
                else => .unknown,
            },

            else => .unknown,
        };
    }
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

/// Convert reg bits to segment register
fn bitsToSegReg(reg: u2) Register {
    return @intToEnum(Register, @enumToInt(Register.es) + reg);
}

/// Source or destination for an instruction.
const SrcDst = struct {
    op0: ?Operand,
    op1: ?Operand,
    op2: ?Operand,
    size: Size,

    const Size = enum {
        byte,
        word,
        // some instructions are only accepted by nasm if you *don't* specify a memory size
        unspecified,
    };

    pub fn init(comptime num: u8, inits: [num]Operand, size: Size) SrcDst {
        var result = SrcDst{
            .op0 = null,
            .op1 = null,
            .op2 = null,
            .size = size,
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

/// The main instruction struct. Each instruction is decoded into an instance of this.
const Instruction = struct {
    mnemonic: Mnemonic,
    /// Destination operand(s). Some instructions only make use of 1 or 2 operands, in
    /// which case the decoder will fill the buffer with `Operand.none`.
    destination: SrcDst,
    /// Source operands(s). Uses the same `none` packing logic as `destination`.
    source: SrcDst,
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
        var src_ptr: *const SrcDst = undefined; // set in block below
        var dst_ptr: *const SrcDst = undefined; // set in block below

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
            // TODO make this check less cryptic. it's checking for a mov imm opcode or an
            // arithmetic imm opcode.
            // TODO three flags?? getting dangerous. untangle all this mess once the tests
            // pass.
            const has_immediate_mov = byte0 & 0b11111110 == 0b11000110;
            const has_immediate_math = byte0 & 0b11111100 == 0b10000000;
            const is_load_effective =
                Mnemonic.init(bytes[0]) == .lea or
                Mnemonic.init(bytes[0]) == .lds or
                Mnemonic.init(bytes[0]) == .les;

            const d = @truncate(u1, byte0 >> 1);
            const w = @truncate(u1, byte0) | @boolToInt(is_load_effective);

            // [ mod/reg/rm ]
            const byte1 = bytes[1];
            const mod = @truncate(u2, byte1 >> 6);
            const reg = @truncate(u3, byte1 >> 3);
            const rm = @truncate(u3, byte1);

            // all cases read at least 2 bytes
            defer parsed_len += 2;

            // assume that the location described in [mod/reg/rm] and [displo/hi] is
            // the destination for now. if the d bit is set, we will swap them at the
            // very end.

            const displacement_size: u8 = blk: {
                if ((mod == 0b00 and rm != 0b110) or mod == 0b11) {
                    // instruction is two bytes long w/ no displacement, no action
                    // needed here.
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
            defer parsed_len += displacement_size;

            // get the displacement, if applicable
            const displacement = blk: {
                switch (displacement_size) {
                    0 => break :blk Operand{ .none = {} },
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

            // now we can finally get the destination from [mod/reg/rm] and [displo]/[disphi]
            const dst = bitsToSrcDst(rm, mod, w, displacement);

            // now get the source. again, for now, we assume that the source is the
            // reg/mem in [mod/reg/rm] or the immediate in the case of has_immediate.
            // we will check the d bit and swap as necessary at the end.

            const wide_immediate = (has_immediate_mov and w == 1) or (has_immediate_math and w == 1 and d == 0);
            const src_op = blk: {
                // the usage of the d bit here is actually checking what the manual labels
                // as s.
                if (has_immediate_mov or has_immediate_math) {
                    var byte_low: u8 = bytes[2 + displacement_size];
                    var byte_high: u8 = if (w == 1) bytes[2 + displacement_size + 1] else undefined;

                    break :blk if (!wide_immediate)
                        // if w == 1, the destination will be wide, so we should decode it
                        // as a word
                        if (w == 0)
                            Operand{ .imm = .{ .imm16 = @bitCast(i8, byte_low) } }
                        else
                            Operand{ .imm = .{ .imm16 = @as(i16, @bitCast(i8, byte_low)) } }
                    else
                        Operand{ .imm = .{ .imm16 = byte_low | (@as(i16, byte_high) << 8) } };
                } else {
                    break :blk Operand{ .reg = bitsToReg(reg, w) };
                }
            };

            const src_size: u8 = switch (src_op) {
                .imm => @as(u8, 1) + @boolToInt(wide_immediate),
                .reg => 0,
                else => unreachable,
            };
            defer parsed_len += src_size;

            const src = SrcDst.init(1, .{src_op}, if (w == 0) .byte else .word);

            // swap src and dst if the d bit is not set. watch out, in imm to reg/mem,
            // the d bit is always 1.
            src_ptr = &src;
            dst_ptr = &dst;

            const swap = (d == 1 and !(has_immediate_mov or has_immediate_math)) or
                is_load_effective == true;
            if (swap) std.mem.swap(*const SrcDst, &src_ptr, &dst_ptr);
        }

        return Instruction{
            .mnemonic = Mnemonic.init2(bytes[0..2].*),
            .destination = dst_ptr.*,
            .source = src_ptr.*,
            .encoded_bytes = parsed_len,
            .binary_index = idx,
        };
    }
};

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

        switch (byte0) {
            // instructions that fit the initFrom6Arith pattern
            0b10001000...0b10001011, // mov reg/mem to/from reg,
            0b11000110...0b11000111, // mov imm to reg/mem,
            0b00000000...0b00000011, // add reg/mem with reg to reg/mem,
            0b00010000...0b00010011, // adc reg/mem with reg to reg/mem,
            0b00101000...0b00101011, // sub reg/mem from reg to reg/mem,
            0b00011000...0b00011011, // sbb reg/mem from reg to reg/mem,
            0b00111000...0b00111011, // cmp reg/mem with reg,
            0b10000000...0b10000011, // arith* imm to reg/mem (*add, adc, sub, sbb, cmp)
            0b10000110...0b10000111, // xchg reg/mem with reg
            0b10001101, // lea
            0b11000101, // lds
            0b11000100, // les
            => {
                const end = @min(self.bytes.len, self.index + 6);
                var i = Instruction.initFrom6Arith(self.bytes[self.index..end], self.index);
                defer self.index += i.encoded_bytes;
                if (i.mnemonic == .lds or i.mnemonic == .les) {
                    // nasm does *not* want you to specify the size of the memory address
                    // for these instructions...
                    i.source.size = .unspecified;
                }
                return i;
            },

            // * imm with ax
            0b00000100...0b00000101, // *add
            0b00010100...0b00010101, // *adc
            0b00101100...0b00101101, // *sub
            0b00011100...0b00011101, // *sbb
            0b00111100...0b00111101, // *cmp
            => {
                const w = @truncate(u1, byte0);
                const byte1 = self.bytes[starting_index + 1];
                const byte2 = if (w == 1) self.bytes[starting_index + 2] else undefined;

                const imm = if (w == 0)
                    Operand{ .imm = .{ .imm16 = @bitCast(i8, byte1) } }
                else
                    Operand{ .imm = .{ .imm16 = @as(i16, byte2) << 8 | byte1 } };

                const reg = if (w == 0)
                    Operand{ .reg = .al }
                else
                    Operand{ .reg = .ax };

                defer self.index += @as(u8, 2) + w;

                return Instruction{
                    .mnemonic = Mnemonic.init(byte0),
                    .destination = SrcDst.init(1, .{reg}, if (w == 0) .byte else .word),
                    .source = SrcDst.init(1, .{imm}, if (w == 0) .byte else .word),
                    .encoded_bytes = @as(u8, 2) + w,
                    .binary_index = self.index,
                };
            },

            0b11111110...0b11111111, // push/inc/dec reg/mem
            0b10001111, // pop reg/mem
            0b11110110...0b11110111, // neg
            => {
                const end = @min(self.bytes.len, self.index + 6);
                var i = Instruction.initFrom6Arith(self.bytes[self.index..end], self.index);
                i.destination = SrcDst.init(0, .{}, .byte);
                defer self.index += i.encoded_bytes;
                return i;
            },

            // push/pop/incdec register oooooreg
            0b01010000...0b01011111, // push/pop
            0b01000000...0b01000111, // inc
            0b01001000...0b01001111, // dec
            => {
                // w is implicitly 1
                const reg = bitsToReg(@truncate(u3, byte0), 1);

                defer self.index += 1;
                return Instruction{
                    .mnemonic = Mnemonic.init(byte0),
                    .destination = SrcDst.init(1, .{.{ .reg = reg }}, .byte),
                    .source = SrcDst.init(0, .{}, .byte),
                    .encoded_bytes = 1,
                    .binary_index = self.index,
                };
            },

            // mov imm to reg
            // 1011wreg
            0b10110000...0b10111111 => {
                const w = @truncate(u1, byte0 >> 3);
                const reg_bits = @truncate(u3, byte0);
                defer self.index += @as(u8, 2) + w;
                const byte1 = self.bytes[starting_index + 1];
                const imm = blk: {
                    const byte2 = if (w == 1) self.bytes[starting_index + 2] else undefined;
                    break :blk if (w == 0) Operand{ .imm = .{ .imm16 = @bitCast(i8, byte1) } } else Operand{ .imm = .{ .imm16 = (@as(i16, byte2) << 8) | byte1 } };
                };

                const reg = bitsToReg(reg_bits, w);
                const reg_op = Operand{ .reg = reg };

                return Instruction{
                    .mnemonic = .mov,
                    .destination = SrcDst.init(1, .{reg_op}, if (w == 0) .byte else .word),
                    .source = SrcDst.init(1, .{imm}, if (w == 0) .byte else .word),
                    .encoded_bytes = @as(u8, 2) + w,
                    .binary_index = self.index,
                };
            },

            // mov ax/mem to ax/mem
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
                const reg_ops = SrcDst.init(1, .{.{ .reg = reg }}, .byte);

                const addr8 = byte1;
                const addr16 = (@as(u16, byte2) << 8) | addr8;

                const mem_ops = if (w == 0)
                    SrcDst.init(2, .{ .{ .imm = .{ .imm16 = @bitCast(i8, addr8) } }, .{ .imm = .{ .imm16 = 0 } } }, .byte)
                else
                    SrcDst.init(2, .{ .{ .imm = .{ .imm16 = @bitCast(i16, addr16) } }, .{ .imm = .{ .imm16 = 0 } } }, .word);

                var dst = &reg_ops;
                var src = &mem_ops;
                if (not_d == 1) std.mem.swap(*const SrcDst, &dst, &src);

                return Instruction{
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
                const reg = bitsToReg(@truncate(u3, byte0), 1);
                return Instruction{
                    .mnemonic = .xchg,
                    .destination = SrcDst.init(1, .{.{ .reg = .ax }}, .word),
                    .source = SrcDst.init(1, .{.{ .reg = reg }}, .word),
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

                const jump_amount = SrcDst.init(1, .{.{ .imm = .{ .inst_addr = @bitCast(i8, byte1) } }}, .byte);

                return Instruction{
                    .mnemonic = Mnemonic.init(byte0),
                    .destination = jump_amount,
                    .source = SrcDst.init(0, .{}, .byte),
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
                const has_data = (byte0 & 0b0001000) == 0;
                const w = @truncate(u1, byte0);
                defer self.index += @as(u8, 1) + @boolToInt(has_data);

                const dest = if (w == 1)
                    SrcDst.init(1, .{.{ .reg = .ax }}, .word)
                else
                    SrcDst.init(1, .{.{ .reg = .al }}, .byte);

                const src = blk: {
                    if (has_data) {
                        const byte1 = @bitCast(i8, self.bytes[self.index + 1]);
                        break :blk SrcDst.init(1, .{.{ .imm = .{ .imm16 = byte1 } }}, .byte);
                    } else {
                        break :blk SrcDst.init(1, .{.{ .reg = .dx }}, .word);
                    }
                };

                const mn = Mnemonic.init(byte0);
                var src_ptr = &src;
                var dst_ptr = &dest;
                if (mn == .out) std.mem.swap(*const SrcDst, &src_ptr, &dst_ptr);

                return Instruction{
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
                const reg = bitsToSegReg(@truncate(u2, byte0 >> 3));
                return Instruction{
                    .mnemonic = Mnemonic.init(byte0),
                    .destination = SrcDst.init(1, .{.{ .reg = reg }}, .word),
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
            => {
                defer self.index += 1;
                return Instruction{
                    .mnemonic = Mnemonic.init(byte0),
                    .destination = SrcDst.init(0, .{}, undefined),
                    .source = SrcDst.init(0, .{}, undefined),
                    .encoded_bytes = 1,
                    .binary_index = self.index,
                };
            },

            else => {
                defer self.index += 1;
                return Instruction{
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

    const size: SrcDst.Size = if (w == 0) .byte else .word;

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

/// Interpret `ops` as the source/destination operands of some instruction. Compose the
/// operands into a single string that is appropriate for the source/destination of some
/// instruction and write the string to `writer`.
///
/// TODO refactor this
fn writeInstSrcDst(inst: Instruction, srcdst: SrcDst, labels: std.AutoHashMap(usize, usize), writer: anytype) !void {
    const is_single_reg_srcdst = srcdst.numOps() == 1 and
        std.meta.activeTag(srcdst.op0.?) == .reg;
    const is_inst_addr = srcdst.numOps() == 1 and
        std.meta.activeTag(srcdst.op0.?) == .imm and
        std.meta.activeTag(srcdst.op0.?.imm) == .inst_addr;

    // TODO this can probably be simplified. seems like the only time we *should* print
    // sizes is a single literal?
    if (!is_single_reg_srcdst and !is_inst_addr and srcdst.size != .unspecified)
        try writer.print("{s} ", .{@tagName(srcdst.size)});

    if (srcdst.numOps() > 1)
        try writer.print("[", .{});

    const ops = [3]?Operand{ srcdst.op0, srcdst.op1, srcdst.op2 };

    for (ops, 0..) |mop, i| {
        if (mop == null) break;
        const op = mop.?;
        switch (op) {
            .none => {},
            .reg => |r| {
                if (i > 0) try writer.print("+", .{});
                try writer.print("{s}", .{@tagName(r)});
            },
            .imm => |ival| {
                switch (ival) {
                    .imm16 => |pval| {
                        if (i > 0) try writer.print("+", .{});
                        // TODO print in hex with dec in comment
                        try writer.print("{}", .{pval});
                    },
                    .inst_addr => |rel_addr| {
                        const abs_addr = @intCast(usize, @intCast(i64, inst.binary_index) + rel_addr + 2);
                        try writer.print("L{?}", .{labels.get(abs_addr)});
                    },
                }
            },
        }
    }

    if (srcdst.numOps() > 1)
        try writer.print("]", .{});
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
        try writer.print("{s} ", .{@tagName(inst.mnemonic)});
        const has_dest = inst.destination.numOps() > 0;
        const has_src = inst.source.numOps() > 0;
        if (has_dest)
            try writeInstSrcDst(inst, inst.destination, labels, writer);
        if (has_dest and has_src)
            try writer.print(", ", .{});
        if (has_src)
            try writeInstSrcDst(inst, inst.source, labels, writer);
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

test "e2e 0041" {
    const alctr = std.testing.allocator;
    try e2eTest("listing_0041_add_sub_cmp_jnz", alctr);
}

test "e2e push pop" {
    const alctr = std.testing.allocator;
    try e2eTest("push_pop", alctr);
}

test "e2e xchg" {
    const alctr = std.testing.allocator;
    try e2eTest("xchg", alctr);
}

test "e2e in out" {
    const alctr = std.testing.allocator;
    try e2eTest("in_out", alctr);
}

test "e2e loads" {
    const alctr = std.testing.allocator;
    try e2eTest("loads", alctr);
}

test "e2e adds" {
    const alctr = std.testing.allocator;
    try e2eTest("adds", alctr);
}

test "e2e subs" {
    const alctr = std.testing.allocator;
    try e2eTest("subs", alctr);
}

// test "e2e 0042" {
//     const alctr = std.testing.allocator;
//     try e2eTest("listing_0042_completionist_decode", alctr);
// }

const std = @import("std");
const expectEq = std.testing.expectEqual;
const expect = std.testing.expect;
