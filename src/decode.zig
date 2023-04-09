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

/// convert reg bits and the w flag to a register name
fn bitsToReg(reg: u3, w: u1) Register {
    // see table 4-9 in the 8086 users manual. we combine the bits into a 4 bit number,
    // taking w to be the most significant bit
    var i: u4 = w;
    i <<= 3;
    i |= reg;
    return @intToEnum(Register, i);
}

pub fn decodeAndPrintFile(filename: []const u8, writer: anytype, alctr: std.mem.Allocator) !void {
    var file = try std.fs.cwd().openFile(filename, .{});
    defer file.close();

    // for now, only support 1MB max. there's no technical reason for this, i just picked
    // it to get the initial version of this function done without thinking. ;)
    const txt = file.readToEndAlloc(alctr, 1024 * 1024) catch |e| switch (e) {
        error.FileTooBig => {
            std.debug.print("Only input files up to 1MB are supported.\n", .{});
            return e;
        },
        else => return e,
    };
    defer alctr.free(txt);

    try writer.print("; disassembly of {s}:\n", .{filename});
    try writer.print("bits 16\n\n", .{});

    var i: usize = 0;
    while (i < txt.len) : (i += 1) {
        // for now, assume every instruction is a register to register mov.
        const byte0 = txt[i];
        const opcode = @truncate(u6, byte0 >> 2);
        const d = @truncate(u1, byte0 >> 1);
        const w = @truncate(u1, byte0);

        // reg/mem to/from reg mov
        std.debug.assert(opcode == 0b100010);

        i += 1;
        const byte1 = txt[i];
        const mod = @truncate(u2, byte1 >> 6);
        const reg = @truncate(u3, byte1 >> 3);
        const reg_or_mem = @truncate(u3, byte1);

        std.debug.assert(mod == 0b11);
        // TODO make these const
        const dst = if (d == 1) bitsToReg(reg, w) else bitsToReg(reg_or_mem, w);
        const src = if (d == 1) bitsToReg(reg_or_mem, w) else bitsToReg(reg, w);

        // try writer.print("; 0b{b:0<8} 0b{b:0<8}\n", .{ byte0, byte1 });
        try writer.print("mov {s}, {s}\n", .{ @tagName(dst), @tagName(src) });
    }
}

test "lookupRegister" {
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

const std = @import("std");
const expectEq = std.testing.expectEqual;
