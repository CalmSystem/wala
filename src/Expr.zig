const u = @import("util.zig");
const std = @import("std");

const Expr = @This();

val: Val,
at: ?Range = null,

pub fn deinit(e: Expr, allocator: std.mem.Allocator) void {
    e.val.deinit(allocator);
}

const Range = struct {
    offset: usize,
    len: usize,

    pub fn view(r: Range, bytes: u.Bin) u.Bin {
        return bytes[r.offset..r.end()];
    }
    pub fn end(r: Range) usize {
        return r.offset + r.len;
    }
};

pub const Format = enum {
    /// Compact S-expr
    compact,
    /// Tree S-expr
    tree,
    /// Human S-expr
    human,
    /// Sweet-expr
    sweet,
};

pub const Val = union(enum) {
    list: []Expr,
    keyword: u.Txt,
    string: u.Bin, //MAYBE: split string in { name, binary }
    id: u.Txt,

    fn deinit(self: Val, allocator: std.mem.Allocator) void {
        switch (self) {
            .list => |exprs| {
                for (exprs) |expr|
                    expr.deinit(allocator);
                allocator.free(exprs);
            },
            else => {},
        }
    }

    fn asText(self: Val) ?u.Txt {
        return switch (self) {
            .keyword, .string, .id => |str| str,
            else => null,
        };
    }

    pub const Func = struct {
        name: u.Txt,
        id: ?u.Txt = null,
        args: []Expr = &[_]Expr{},
    };
    pub fn asFunc(self: Val) ?Func {
        switch (self) {
            .list => |exprs| {
                if (exprs.len > 0) {
                    if (exprs[0].val.asKeyword()) |name| {
                        const id = if (exprs.len > 1) exprs[1].val.asId() else null;
                        const offset: usize = if (id != null) 2 else 1;
                        return Func{ .name = name, .id = id, .args = exprs[offset..] };
                    }
                }
            },
            else => {},
        }
        return null;
    }

    pub inline fn asKeyword(self: Val) ?u.Txt {
        return switch (self) {
            .keyword => |keyword| keyword,
            else => null,
        };
    }
    pub inline fn asId(self: Val) ?u.Txt {
        return switch (self) {
            .id => |id| id,
            else => null,
        };
    }
    pub inline fn asString(self: Val) ?u.Bin {
        return switch (self) {
            .string => |str| str,
            else => null,
        };
    }

    pub fn shallowEql(self: Val, other: Val) bool {
        if (@enumToInt(self) != @enumToInt(other)) return false;
        const str = self.asText() orelse return false;
        return u.strEql(str, other.asText().?);
    }

    pub fn format(
        self: Val,
        comptime sfmt: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        const fmt = if (comptime sfmt.len > 0)
            comptime (std.meta.stringToEnum(Format, sfmt) orelse @panic("Not a valid Expr.Format"))
        else
            .compact;
        try self.print(fmt, writer, 0);
    }
    pub fn print(self: Val, comptime fmt: Format, writer: anytype, prev_depth: usize) @TypeOf(writer).Error!void {
        switch (self) {
            .list => |list| {
                switch (fmt) {
                    .compact => {
                        try writer.writeByte('(');
                        for (list) |expr, i| {
                            try expr.val.print(fmt, writer, 0);
                            if (i + 1 < list.len)
                                try writer.writeByte(' ');
                        }
                        try writer.writeByte(')');
                    },
                    .tree, .human, .sweet => {
                        const depth = prev_depth + 2;
                        if (fmt != .sweet)
                            try writer.writeByte('(');
                        if (list.len > 0)
                            try list[0].val.print(fmt, writer, depth);
                        var i: usize = 1;
                        for (list) |expr| {
                            if (fmt != .tree and list[i - 1].val.asText() != null and list[i].val.asText() != null) {
                                try writer.writeByte(' ');
                            } else {
                                try writer.writeByte('\n');
                                try writer.writeByteNTimes(' ', depth);
                            }
                            try expr.val.print(fmt, writer, depth);
                        }
                        if (fmt != .sweet) {
                            try writer.writeByte(')');
                        }
                    },
                }
            },
            .keyword => |str| {
                try writer.writeAll(str);
            },
            .id => |str| {
                try writer.writeByte('$');
                try writer.writeAll(str);
            },
            .string => |str| {
                try writer.writeByte('"');
                try writer.writeAll(str);
                try writer.writeByte('"');
            },
        }
    }
};

pub inline fn format(
    self: Expr,
    comptime sfmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    return try self.val.format(sfmt, options, writer);
}
pub fn print(self: Expr, fmt: Format, writer: anytype) @TypeOf(writer).Error!void {
    switch (fmt) {
        .compact => try self.val.print(.compact, writer, 0),
        .tree => try self.val.print(.tree, writer, 0),
        .human => try self.val.print(.human, writer, 0),
        .sweet => try self.val.print(.sweet, writer, 0),
    }
}
