//! decoding machine language into assembly

/// 8086 register names. the order of the values in this enum has been chosen to match
/// table 4-9 in the 8086 users manual (page 263). to make the order, it is assumed that
/// the W bit is the most significant bit.
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

fn lookupRegister(reg: u3, w: u1) Register {
    var i: u4 = w;
    i <<= 3;
    i |= reg;
    return @intToEnum(Register, i);
}

const register_strings: [16]*const [2:0]u8 = .{
    "al",
    "cl",
    "dl",
    "bl",
    "ah",
    "ch",
    "dh",
    "bh",
    "ax",
    "cx",
    "dx",
    "bx",
    "sp",
    "bp",
    "si",
    "di",
};

fn regToString(reg: Register) *const [2:0]u8 {
    return register_strings[@enumToInt(reg)];
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
        const dst = if (d == 1) lookupRegister(reg, w) else lookupRegister(reg_or_mem, w);
        const src = if (d == 1) lookupRegister(reg_or_mem, w) else lookupRegister(reg, w);

        // try writer.print("; 0b{b:0<8} 0b{b:0<8}\n", .{ byte0, byte1 });
        try writer.print("mov {s}, {s}\n", .{ regToString(dst), regToString(src) });
    }
}

test "lookupRegister" {
    // from table 4-9 in intel 8086 users manual

    try expectEq(Register.al, lookupRegister(0b000, 0));
    try expectEq(Register.cl, lookupRegister(0b001, 0));
    try expectEq(Register.dl, lookupRegister(0b010, 0));
    try expectEq(Register.bl, lookupRegister(0b011, 0));
    try expectEq(Register.ah, lookupRegister(0b100, 0));
    try expectEq(Register.ch, lookupRegister(0b101, 0));
    try expectEq(Register.dh, lookupRegister(0b110, 0));
    try expectEq(Register.bh, lookupRegister(0b111, 0));

    try expectEq(Register.ax, lookupRegister(0b000, 1));
    try expectEq(Register.cx, lookupRegister(0b001, 1));
    try expectEq(Register.dx, lookupRegister(0b010, 1));
    try expectEq(Register.bx, lookupRegister(0b011, 1));
    try expectEq(Register.sp, lookupRegister(0b100, 1));
    try expectEq(Register.bp, lookupRegister(0b101, 1));
    try expectEq(Register.si, lookupRegister(0b110, 1));
    try expectEq(Register.di, lookupRegister(0b111, 1));
}

const std = @import("std");
const expectEq = std.testing.expectEqual;
