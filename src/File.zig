const std = @import("std");
const SParser = @import("SParser.zig");
const SweetParser = @import("SweetParser.zig");
const Expr = SParser.Expr;
const TextIterator = SParser.TextIterator;

const File = @This();

realpath: []const u8,
text: []const u8,
arena: std.heap.ArenaAllocator,

pub fn init(path: []const u8, child_allocator: std.mem.Allocator) !File {
    var arena = std.heap.ArenaAllocator.init(child_allocator);
    return load(path, &arena);
}
pub fn load(path: []const u8, arena: *std.heap.ArenaAllocator) !File {
    const allocator = arena.allocator();

    const realpath = try std.fs.realpathAlloc(allocator, path);

    const file = try std.fs.openFileAbsolute(realpath, .{ .read = true });
    defer file.close();

    const size = try file.getEndPos();
    const text = try file.readToEndAlloc(allocator, size);
    _ = try file.read(text);

    return File{ .realpath = realpath, .text = text, .arena = arena.* };
}
pub fn deinit(self: File) void {
    self.arena.deinit();
}

pub fn readS(self: *File) ![]Expr {
    var iter = TextIterator.init(self.text);
    return SParser.parseAll(&iter, self.arena.allocator());
}
pub fn read(self: *File) ![]Expr {
    var iter = TextIterator.init(self.text);
    return SweetParser.parseAll(&iter, self.arena.allocator());
}

pub const ReadErr = struct {
    kind: anyerror,
    at: usize,

    pub fn print(self: ReadErr, file: File) void {
        const point = file.linePoint(self.at);
        std.debug.print("{/}{}\n{^}", .{ point, self.kind, point });
    }
};
pub const ReadResult = union(enum) {
    ok: []Expr,
    err: ReadErr,
};
pub fn tryReadS(self: *File) ReadResult {
    var iter = TextIterator.init(self.text);
    const exprs = SParser.parseAll(&iter, self.arena.allocator())
        catch |err| return .{ .err = .{ .kind = err, .at = iter.peek().offset } };
    return .{ .ok = exprs };
}
pub fn tryRead(self: *File) ReadResult {
    var iter = TextIterator.init(self.text);
    const exprs = SweetParser.parseAll(&iter, self.arena.allocator())
        catch |err| return .{ .err = .{ .kind = err, .at = iter.peek().offset } };
    return .{ .ok = exprs };
}

pub inline fn linePoint(self: File, at: usize) LinePoint {
    return LinePoint.init(self.text, at, self.realpath);
}

/// Human readable file position
pub const Position = struct {
    column: usize = 1,
    line: usize = 1,
};

/// Human readable file position pointer
pub const LinePoint = struct {
    path: []const u8,
    text: []const u8,
    offset: usize,
    at: Position,

    pub fn init(text: []const u8, offset: usize, path: []const u8) LinePoint {
        var iter = TextIterator.Inner{ .bytes = text };
        var p: Position = .{ };
        var last_line: usize = 0;
        while (iter.next()) |cp| {
            if (cp.offset >= offset)
                break;
            if (cp.scalar == '\n') {
                last_line = cp.offset + cp.bytes.len;
                p.line += 1;
                p.column = 1;
            } else {
                p.column += 1;
            }
        }

        const end_line = last_line + (TextIterator.indexOfCodePoint(text[last_line..], '\n') orelse text[last_line..].len);
        return LinePoint{ .path = path, .text = text[last_line..end_line], .offset = offset - last_line, .at = p };
    }

    pub fn format(
        self: LinePoint,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        const link = std.mem.eql(u8, fmt, "/");
        const line = std.mem.eql(u8, fmt, "^");
        if (!line)
            try writer.print("{s}:{}:{}: ", .{ self.path, self.at.line, self.at.column });
        if (!(link or line))
            try writer.print("{s}\n", .{fmt});
        if (!link) {
            try writer.print("{s}\n", .{self.text});
            var i: usize = 1;
            while (i < self.at.column) : (i += 1) {
                try writer.writeByte(options.fill);
            }
            try writer.writeAll("^\n");
        }
    }
};
