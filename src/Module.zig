const std = @import("std");
const Expr = @import("Expr.zig");

pub fn compile(exprs: []Expr, alloc: std.mem.Allocator) !Expr {
    const moduleName = Expr{ .val = .{ .name = "module" } };

    if (exprs.len == 1 and switch (exprs[0].val) {
        .list => |list| list.len > 0 and moduleName.val.shallow_eql(list[0].val),
        else => false
    }) return exprs[0];

    var list = try std.ArrayListUnmanaged(Expr).initCapacity(alloc, exprs.len+1);
    list.appendAssumeCapacity(moduleName);
    list.appendSliceAssumeCapacity(exprs);

    return Expr{ .val = .{ .list = list.items } };
}
