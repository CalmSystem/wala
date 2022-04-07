const u = @import("util.zig");
const std = @import("std");
const SParser = @import("SParser.zig");
const SweetParser = @import("SweetParser.zig");
const Expr = SParser.Expr;
const TextIterator = SParser.TextIterator;

pub const Type = enum { wasm, text };

pub const Any = union(Type) {
    wasm: Wasm,
    text: Text,

    pub fn read(path: u.Txt, allocator: std.mem.Allocator) !Any {
        const safepath = try std.fs.realpathAlloc(allocator, path);

        const file = try std.fs.openFileAbsolute(safepath, .{ .read = true });
        defer file.close();

        const size = try file.getEndPos();
        const bytes = try file.readToEndAlloc(allocator, size);
        _ = try file.read(bytes);

        if (std.mem.startsWith(u8, bytes, &std.wasm.magic))
            return Any{ .wasm = .{ .realpath = safepath, .bytes = bytes, .allocator = allocator } };

        return Any{ .text = .{ .realpath = safepath, .text = try u.toTxt(bytes), .allocator = allocator } };
    }
    pub fn deinit(self: Any) void {
        switch (self) {
            .wasm => |wasm| wasm.deinit(),
            .text => |text| text.deinit(),
        }
    }

    pub inline fn realpath(self: Any) u.Txt {
        return switch (self) {
            .wasm => |wasm| wasm.realpath,
            .text => |text| text.realpath,
        };
    }
};

pub const read = Any.read;

pub const Wasm = struct {
    realpath: u.Txt,
    bytes: u.Bin,
    allocator: std.mem.Allocator,

    pub fn deinit(self: Wasm) void {
        self.allocator.free(self.bytes);
        self.allocator.free(self.realpath);
    }
};

pub const Text = struct {
    realpath: u.Txt,
    text: u.Txt,
    allocator: std.mem.Allocator,

    pub fn deinit(self: Text) void {
        self.allocator.free(self.text);
        self.allocator.free(self.realpath);
    }

    inline fn iter(self: Text) TextIterator {
        return TextIterator.unsafeInit(self.text);
    }
    inline fn parseAs(comptime sweet: bool) fn (*TextIterator, std.mem.Allocator) SweetParser.Error![]Expr {
        return if (comptime sweet) SweetParser.parseAll else SParser.parseAll;
    }

    pub fn readAs(self: Text, comptime sweet: bool) ![]Expr {
        var iter_ = self.iter();
        return parseAs(sweet)(&iter_, self.allocator);
    }
    pub inline fn read(self: Text) ![]Expr {
        return self.readAs(true);
    }

    pub fn tryReadAs(self: Text, comptime sweet: bool) ReadResult {
        var iter_ = self.iter();
        const exprs = parseAs(sweet)(&iter_, self.allocator) catch |err|
            return .{ .err = .{ .kind = err, .at = iter_.peek().offset, .file = &self } };
        return .{ .ok = exprs };
    }
    pub inline fn tryRead(self: Text) ReadResult {
        return self.tryReadAs(true);
    }

    pub inline fn linePoint(self: Text, at: usize) LinePoint {
        return LinePoint.init(self.text, at, self.realpath);
    }
};

pub const ErrPoint = struct {
    kind: anyerror,
    at: usize,
    file: *const Text,

    pub fn format(
        self: ErrPoint,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        const point = self.file.linePoint(self.at);
        try writer.print("{/}{}\n{^}", .{ point, self.kind, point });
    }
};
pub const ReadResult = union(enum) {
    ok: []Expr,
    err: ErrPoint,
};

/// Human readable text file position
pub const Position = struct {
    column: usize = 1,
    line: usize = 1,
};

/// Human readable text file position pointer
pub const LinePoint = struct {
    path: u.Txt,
    line: u.Txt,
    offset: usize,
    at: Position,

    pub fn init(text: u.Txt, offset: usize, path: u.Txt) LinePoint {
        std.debug.assert(u.isTxt(text));
        var iter = TextIterator{ .cur = null, .bytes = text };
        var p: Position = .{};
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
        return LinePoint{ .path = path, .line = text[last_line..end_line], .offset = offset - last_line, .at = p };
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
            try writer.print("{s}\n", .{self.line});
            var i: usize = 1;
            while (i < self.at.column) : (i += 1) {
                try writer.writeByte(options.fill);
            }
            try writer.writeAll("^");
        }
    }
};
