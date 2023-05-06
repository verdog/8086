//! Functions for turning unreadable things into human readable things

pub fn writeInst(inst: dec.Instruction, labels: ?std.AutoHashMap(usize, usize), writer: anytype) !void {
    for (inst.prefixes) |p|
        switch (p) {
            .none => break,
            .cnst => |c| try writer.print("{s} ", .{@tagName(c)}),
            .seg => {}, // segment prefixes should not apply to instructions
            .imm => unreachable,
        };
    try writer.print("{s}", .{@tagName(inst.mnemonic)});
    const has_dest = inst.destination.numOps() > 0;
    const has_src = inst.source.numOps() > 0;
    if (has_dest) {
        try writer.print(" ", .{});
        try writeInstSrcDst(inst, inst.destination, labels, writer);
    }
    if (has_dest and has_src)
        try writer.print(",", .{});
    if (has_src) {
        try writer.print(" ", .{});
        try writeInstSrcDst(inst, inst.source, labels, writer);
    }
}

/// Compose the operands of `inst` into a single string that is appropriate for the
/// source/destination of some instruction and write the string to `writer`.
///
/// If the instruction is a jump instruction, the function will use `labels`, a map of
/// instruction addresses to unique label ids, to generate labels as arguments. If no
/// labels are given, the jump instruction's destination will be printed as a number.
pub fn writeInstSrcDst(inst: dec.Instruction, srcdst: dec.SrcDst, labels: ?std.AutoHashMap(usize, usize), writer: anytype) !void {
    for (srcdst.prefixes) |p|
        switch (p) {
            .none => break,
            .cnst => |c| try writer.print("{s} ", .{@tagName(c)}),
            .seg => |s| try writer.print("{s}:", .{@tagName(s)}),
            .imm => |i| try writer.print("{}: ", .{i}),
        };

    if (srcdst.numOps() > 1)
        try writer.print("[", .{});

    const ops = [3]?dec.Operand{ srcdst.op0, srcdst.op1, srcdst.op2 };

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
                        if (labels) |ls| {
                            try writer.print("L{?}", .{ls.get(abs_addr)});
                        } else {
                            try writer.print("0x{x:0>2}", .{rel_addr});
                        }
                    },
                }
            },
        }
    }

    if (srcdst.numOps() > 1)
        try writer.print("]", .{});
}

const std = @import("std");
const dec = @import("decode.zig");
