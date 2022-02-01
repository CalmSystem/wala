const std = @import("std");
const ziglyph = @import("ziglyph");
const Parser = @import("Parser.zig");

const FileReader = @This();
normalizer: ziglyph.Normalizer,

pub fn init(allocator: std.mem.Allocator) !FileReader {
    return FileReader{ .normalizer = try ziglyph.Normalizer.init(allocator) };
}
pub fn deinit(self: *FileReader) void {
    self.normalizer.deinit();
}

pub fn read(self: *FileReader, path: []const u8) !File {
    return self.read2(path, std.heap.page_allocator);
}
pub fn read2(self: *FileReader, path: []const u8, tmp_allocator: std.mem.Allocator) !File {
    const realpath = try std.fs.realpathAlloc(self.normalizer.arena.allocator(), path);

    const file = try std.fs.openFileAbsolute(realpath, .{ .read = true });
    defer file.close();

    const file_size = try file.getEndPos();
    const file_buffer = try file.readToEndAlloc(tmp_allocator, file_size);
    defer tmp_allocator.destroy(file_buffer.ptr);
    _ = try file.read(file_buffer);

    return self.load(realpath, file_buffer);
}
pub fn load(self: *FileReader, realpath: []const u8, bytes: []const u8) !File {
    return File{ .path = realpath, .text = try self.normalizer.normalizeTo(.komposed, bytes) };
}

pub const Parsed = struct {
    file: File,
    tree: Parser.Block,
};
const ParseResult = union(enum) {
    ok: Parsed,
    err: Err,

    const Err = struct {
        kind: anyerror,
        at: LinePoint,
    };
};
pub fn tryParse(self: *FileReader, path: []const u8) ParseResult {
    const file = self.read(path) catch |err| {
        const point = LinePoint{ .path = path, .text = "", .offset = 0, .at = Position{ .line = 0, .column = 0 } };
        if (@errorReturnTrace()) |trace| {
            std.debug.dumpStackTrace(trace.*);
        }
        return ParseResult{ .err = ParseResult.Err{ .kind = err, .at = point } };
    };
    return switch (file.tryParse(self.normalizer.allocator)) {
        .ok => |ok| ParseResult{ .ok = Parsed{ .file = file, .tree = ok } },
        .err => |err| ParseResult{ .err = ParseResult.Err{ .kind = err.kind, .at = file.linePoint(err.offset) } },
    };
}

pub const File = struct {
    path: []const u8,
    text: []const u8,

    pub fn parse(self: File, allocator: std.mem.Allocator) !Parser.File {
        return Parser.parse(self.text, allocator);
    }
    pub fn tryParse(self: File, allocator: std.mem.Allocator) Parser.Result {
        return Parser.tryParse(self.text, allocator);
    }
    pub fn linePoint(self: File, offset: usize) LinePoint {
        return LinePoint.init(self.text, offset, self.path);
    }
};

/// Human readable file position
pub const Position = struct {
    column: usize = 1,
    line: usize = 1,
};

pub fn indexOfCodePoint(str: []const u8, scalar: u21) ?usize {
    var cp_iter = ziglyph.CodePointIterator{ .bytes = str };
    while (cp_iter.next()) |cp| {
        if (cp.scalar == scalar)
            return cp.offset;
    }
    return null;
}

pub const LinePoint = struct {
    path: []const u8,
    text: []const u8,
    offset: usize,
    at: Position,

    pub fn init(text: []const u8, offset: usize, path: []const u8) LinePoint {
        var cp_iter = ziglyph.CodePointIterator{ .bytes = text };
        var p = Position{};
        var last_line: usize = 0;
        while (cp_iter.next()) |cp| {
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
        const end_line = last_line + (indexOfCodePoint(text[last_line..], '\n') orelse text[last_line..].len);
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
