const std = @import("std");
const Expr = @import("Expr.zig");

arena: std.heap.ArenaAllocator,

const Self = @This();

pub fn init(child_allocator: std.mem.Allocator) Self {
    return .{ .arena = std.heap.ArenaAllocator.init(child_allocator) };
}
pub fn deinit(self: Self) void {
    self.arena.deinit();
}


