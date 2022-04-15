const std = @import("std");
const u = @import("util.zig");
const File = @import("File.zig");
const IR = @import("IR.zig");
const Wat = @import("Wat.zig");
const Wasm = @import("Wasm.zig");

allocator: std.mem.Allocator,
trace: std.ArrayListUnmanaged(u.Txt) = .{},
errAt: fn (arg: ErrArg) void,

const Self = @This();
pub const ErrArg = union(File.Type) {
    text: struct {
        point: File.ErrPoint,
        data: Wat.ErrData = null,
    },
    wasm: struct {
        kind: anyerror,
        at: usize,
        file: *const File.Wasm,
    },
};

pub fn load(self: *Self, entry: u.Txt) !IR.Module {
    //TODO: cwd from trace
    const file = try File.read(entry, self.allocator);
    defer file.deinit();

    return switch (file) {
        .text => |text| switch (text.tryRead()) {
            .ok => |exprs| switch (Wat.tryLoad(exprs, self.allocator, self)) {
                .ok => |m| m,
                .err => |err| {
                    if (err.at != null and err.at.?.at != null) {
                        self.errAt(.{ .text = .{ .point = .{ .kind = err.kind, .file = &text, .at = err.at.?.at.?.offset }, .data = err.data } });
                    }
                    return err.kind;
                },
            },
            .err => |err| {
                self.errAt(.{ .text = .{ .point = err } });
                return err.kind;
            },
        },
        .wasm => |wasm| switch (Wasm.tryLoad(wasm.bytes, self.allocator)) {
            .ok => |m| m,
            .err => |err| {
                self.errAt(.{ .wasm = .{ .kind = err.kind, .file = &wasm, .at = err.at } });
                return err.kind;
            },
        },
    };
}

pub fn writeWasm(m: IR.Module, writer: anytype) !void {
    return Wasm.emit(m, writer, .{});
}
pub fn writeText(m: IR.Module, writer: anytype, alloc: std.mem.Allocator, fmt: @import("Expr.zig").Format) !void {
    const wat = try Wat.emit(m, alloc, .{});
    defer wat.deinit(alloc);

    try wat.print(fmt, writer);
    try writer.writeByte('\n');
}
