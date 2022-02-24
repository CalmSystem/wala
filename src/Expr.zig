const std = @import("std");

const Expr = @This();

val: Val,
at: ?Range = null,

const Range = struct {
    offset: usize,
    len: usize,

    pub fn view(r: Range, bytes: []const u8) []const u8 {
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
    name: []const u8,
    str: []const u8,
    ident: []const u8,

    fn text(self: Val) ?[]const u8 {
        return switch(self) {
            .name,.str,.ident => |str| str,
            else => null
        };
    }

    pub fn shallow_eql(self: Val, other: Val) bool {
        if (@enumToInt(self) != @enumToInt(other)) return false;
        return if (self.text()) |str|
            std.mem.eql(u8, str, other.text().?)
            else false;
    }

    pub fn format(
        self: Val,
        comptime sfmt: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        const fmt = if (comptime sfmt.len > 0)
            comptime(std.meta.stringToEnum(Format, sfmt) orelse @panic("Not a valid Expr.Format"))
        else .compact;
        try self.print(fmt, writer, 0);
    }
    pub fn print(self: Val, comptime fmt: Format, writer: anytype, prev_depth: usize) @TypeOf(writer).Error!void {
        switch(self) {
            .list => |list| {
                switch(fmt) {
                    .compact => {
                        try writer.writeByte('(');
                        var i: usize = 0;
                        while (i < list.len): (i += 1) {
                            try list[i].val.print(fmt, writer, 0);
                            if (i+1 < list.len)
                                try writer.writeByte(' ');
                        }
                        try writer.writeByte(')');
                    },
                    .tree,.human,.sweet => {
                        const depth = prev_depth+2;
                        if (fmt != .sweet)
                            try writer.writeByte('(');
                        if (list.len > 0)
                            try list[0].val.print(fmt, writer, depth);
                        var i: usize = 1;
                        while (i < list.len): (i += 1) {
                            if (fmt != .tree and list[i-1].val.text() != null and list[i].val.text() != null) {
                                try writer.writeByte(' ');
                            } else {
                                try writer.writeByte('\n');
                                try writer.writeByteNTimes(' ', depth);
                            }
                            try list[i].val.print(fmt, writer, depth);
                        }
                        if (fmt != .sweet) {
                            try writer.writeByte(')');
                        }
                    }
                }
            },
            .name => |str| {
                try writer.writeAll(str);
            },
            .ident => |str| {
                try writer.writeByte('$');
                try writer.writeAll(str);
            },
            .str => |str| {
                try writer.writeByte('"');
                try writer.writeAll(str);
                try writer.writeByte('"');
            }
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
