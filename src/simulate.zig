//! CPU/Memory simulation

const Data = struct {
    //! All simulated data, mainly registers and memory

    // registers

    ax: u16 = 0,
    cx: u16 = 0,
    dx: u16 = 0,
    bx: u16 = 0,
    sp: u16 = 0,
    bp: u16 = 0,
    si: u16 = 0,
    di: u16 = 0,
    es: u16 = 0,
    cs: u16 = 0,
    ss: u16 = 0,
    ds: u16 = 0,

    // TODO memory

    /// Get the value in `reg`. If `reg` is an 8 bit register, the lower 8 bits of the
    /// register are used.
    pub fn getRegister(self: Data, reg: nms.Register) u16 {
        return switch (reg) {
            .al => 0x00ff & self.ax,
            .cl => 0x00ff & self.cx,
            .dl => 0x00ff & self.dx,
            .bl => 0x00ff & self.bx,
            .ah => (0xff00 & self.ax) >> 8,
            .ch => (0xff00 & self.cx) >> 8,
            .dh => (0xff00 & self.dx) >> 8,
            .bh => (0xff00 & self.bx) >> 8,
            inline else => |r| @field(self, @tagName(r)),
        };
    }

    /// Put `value` into `reg`. If `reg` is an 8 bit register, the lower 8 bits of `value`
    /// are used.
    pub fn putRegister(self: *Data, reg: nms.Register, value: u16) void {
        switch (reg) {
            .al => self.ax = (self.ax & 0xff00) | (0x00ff & value),
            .cl => self.cx = (self.cx & 0xff00) | (0x00ff & value),
            .dl => self.dx = (self.dx & 0xff00) | (0x00ff & value),
            .bl => self.bx = (self.bx & 0xff00) | (0x00ff & value),
            .ah => self.ax = (self.ax & 0x00ff) | (value << 8),
            .ch => self.cx = (self.cx & 0x00ff) | (value << 8),
            .dh => self.dx = (self.dx & 0x00ff) | (value << 8),
            .bh => self.bx = (self.bx & 0x00ff) | (value << 8),
            inline else => |r| @field(self, @tagName(r)) = value,
        }
    }
};

pub fn simulateAndPrintFile(filename: []const u8, writer: anytype, alctr: std.mem.Allocator) !void {
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

    try writer.print("simulation of {s}:\n", .{filename});

    var data = Data{};

    var decoder = dec.DecodeIterator.init(asm_bin);
    while (decoder.next()) |inst| {
        switch (inst.mnemonic) {
            .mov => {
                const src = blk: {
                    std.debug.assert(inst.source.numOps() == 1);
                    switch (inst.source.op0.?) {
                        .imm => |i| {
                            break :blk i.imm16;
                        },
                        .none => return error.NoneOperand,
                        else => return error.Unimplemented,
                    }
                };

                const dst = blk: {
                    std.debug.assert(inst.destination.numOps() == 1);
                    switch (inst.destination.op0.?) {
                        .reg => |r| {
                            break :blk r;
                        },
                        .none => return error.NoneOperand,
                        else => return error.Unimplemented,
                    }
                };

                const old = data.getRegister(dst);
                data.putRegister(dst, @bitCast(u16, src));
                try txt.writeInst(inst, null, writer);
                try writer.print(
                    " ; {s}: 0x{x:0>4} -> 0x{x:0>4}\n",
                    .{ @tagName(dst), old, data.getRegister(dst) },
                );
            },
            else => {
                std.debug.print("unimplemented: {s}\n", .{@tagName(inst.mnemonic)});
                return error.Unimplemented;
            },
        }
    }

    try writer.print("\nfinal register values:\n", .{});
    try writer.print("ax: 0x{0x:0>4} ({0})\n", .{data.ax});
    try writer.print("cx: 0x{0x:0>4} ({0})\n", .{data.cx});
    try writer.print("dx: 0x{0x:0>4} ({0})\n", .{data.dx});
    try writer.print("bx: 0x{0x:0>4} ({0})\n", .{data.bx});
    try writer.print("sp: 0x{0x:0>4} ({0})\n", .{data.sp});
    try writer.print("bp: 0x{0x:0>4} ({0})\n", .{data.bp});
    try writer.print("si: 0x{0x:0>4} ({0})\n", .{data.si});
    try writer.print("di: 0x{0x:0>4} ({0})\n", .{data.di});
    try writer.print("es: 0x{0x:0>4} ({0})\n", .{data.es});
    try writer.print("cs: 0x{0x:0>4} ({0})\n", .{data.cs});
    try writer.print("ss: 0x{0x:0>4} ({0})\n", .{data.ss});
    try writer.print("ds: 0x{0x:0>4} ({0})\n", .{data.ds});
}

test "Data.putRegister" {
    var data = Data{};

    // TODO figure out how to introspect/iteratve over all possible
    // enum fields
    inline for (0..20) |i| {
        const sized = @truncate(u16, i);
        const reg = @intToEnum(nms.Register, sized);
        data.putRegister(reg, sized + 1);
        try expectEq(data.getRegister(reg), sized + 1);
    }
}

test "Data.putRegister: handle 8 bit registers" {
    var data = Data{};

    data.putRegister(.al, 0xff01);
    try expectEq(data.getRegister(.al), 1);
    try expectEq(data.getRegister(.ah), 0);

    data.putRegister(.ah, 0xff01);
    try expectEq(data.getRegister(.al), 1);
    try expectEq(data.getRegister(.ah), 1);
    try expectEq(data.getRegister(.ax), 0x0101);

    data.putRegister(.bx, 0x9878);
    try expectEq(data.getRegister(.bl), 0x78);
    try expectEq(data.getRegister(.bh), 0x98);
    try expectEq(data.getRegister(.bx), 0x9878);
}

test "e2e listing_0043_immediate_movs" {
    const alctr = std.testing.allocator;
    try tst.simulateEndToEnd("listing_0043_immediate_movs", alctr);
}

const std = @import("std");
const dec = @import("decode.zig");
const nms = @import("names.zig");
const txt = @import("text.zig");
const tst = @import("test.zig");

const expectEq = std.testing.expectEqual;
const expect = std.testing.expect;
