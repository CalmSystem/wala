const std = @import("std");
const u = @import("util.zig");
const File = @import("File.zig");
const IR = @import("IR.zig");
const Wat = @import("Wat.zig");
const Wasm = @import("Wasm.zig");

allocator: std.mem.Allocator,
trace: std.ArrayListUnmanaged(u.Txt) = .{},
readErr: fn(err: File.ReadErr) void,

const Self = @This();

pub fn load(self: *Self, entry: u.Txt) !IR.Module {
    //TODO: cwd from trace
    const file = try File.read(entry, self.allocator);
    defer file.deinit();

    return switch (file) {
        .text => |text| switch (text.tryRead()) {
            .ok => |exprs| //TODO: Wat.tryLoad and watErr
                try Wat.loadRec(exprs, self.allocator, self),
            .err => |err| {
                self.readErr(err);
                return err.kind;
            }
        },
        .wasm => |wasm| //TODO: Wasm.tryLoad and wasmErr
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
