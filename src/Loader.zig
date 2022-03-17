const std = @import("std");
const u = @import("util.zig");
const File = @import("File.zig");
const IR = @import("IR.zig");
const Wat = @import("Wat.zig");
const Wasm = @import("Wasm.zig");

allocator: std.mem.Allocator,
trace: std.ArrayListUnmanaged(u.Txt) = .{},
errAt: fn(point: ErrPoint, data: ErrData) void,

const Self = @This();
pub const ErrPoint = File.ErrPoint;
pub const ErrData = Wat.ErrData;

pub fn load(self: *Self, entry: u.Txt) !IR.Module {
    //TODO: cwd from trace
    const file = try File.read(entry, self.allocator);
    defer file.deinit();

    return switch (file) {
        .text => |text| switch (text.tryRead()) {
            .ok => |exprs| switch (Wat.tryLoad(exprs, self.allocator, self)) {
                .ok => |m| m,
                .err => |err| {
                    if (err.at != null and err.at.?.at != null) self.errAt(.{
                        .kind = err.kind, .file = &text,
                        .at = err.at.?.at.?.offset
                    }, err.data);
                    return err.kind;
                }
            },
            .err => |err| {
                self.errAt(err, null);
                return err.kind;
            }
        },
        .wasm => |wasm| //MAYBE: Wasm.tryLoad and Self.errAtBin
            try Wasm.load(wasm.bytes, self.allocator),
    };
}

pub fn writeWasm(m: IR.Module, writer: anytype) !void {
    return Wasm.emit(m, @TypeOf(writer), writer, .{ });
}
pub fn writeText(m: IR.Module, writer: anytype, alloc: std.mem.Allocator, fmt: @import("Expr.zig").Format) !void {
    const wat = try Wat.emit(m, alloc, .{ });
    defer wat.deinit(alloc);

    try wat.print(fmt, writer);
}
