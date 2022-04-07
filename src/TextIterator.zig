const std = @import("std");
const u = @import("util.zig");

const CodePoint = struct {
    bytes: u.Txt,
    offset: usize,
    scalar: u21,

    pub fn end(self: CodePoint) usize {
        return self.offset + self.bytes.len;
    }
};

bytes: u.Txt,
i: usize = 0,
cur: ?CodePoint,

const Self = @This();
const eofBytes = &[_]u8{};

pub inline fn init(bytes: u.Bin) !Self {
    return unsafeInit(try u.toTxt(bytes));
}
pub fn unsafeInit(str: u.Txt) Self {
    var self = Self{ .cur = null, .bytes = str };
    self.skip();
    return self;
}

pub fn next(self: *Self) ?CodePoint {
    if (self.i >= self.bytes.len) {
        self.cur = null;
        return null;
    }

    var cp = CodePoint{
        .bytes = undefined,
        .offset = self.i,
        .scalar = undefined,
    };
    const cp_len = std.unicode.utf8ByteSequenceLength(self.bytes[self.i]) catch unreachable;
    self.i += cp_len;
    cp.bytes = self.bytes[self.i - cp_len .. self.i];
    cp.scalar = std.unicode.utf8Decode(cp.bytes) catch unreachable;

    self.cur = cp;
    return self.cur;
}
pub fn skip(self: *Self) void {
    _ = self.next();
}
pub fn peek(self: Self) CodePoint {
    return if (self.cur) |c| c else CodePoint{ .scalar = 0, .offset = self.bytes.len, .bytes = eofBytes };
}

pub fn eol(self: Self) bool {
    return self.cur != null and self.cur.?.scalar == '\n';
}
pub fn eof(self: Self) bool {
    return self.cur == null;
}

pub fn readWhile(self: *Self, comptime pred: fn (u21) bool) u.Txt {
    if (self.cur) |p| {
        while (self.cur != null and pred(self.cur.?.scalar)) {
            self.skip();
        }
        return if (self.cur) |q|
            self.bytes[p.offset..q.offset]
        else
            self.bytes[p.offset..];
    } else return eofBytes;
}

pub fn isSpace(cp: u21) bool {
    if (cp < 0x9 or cp > 0x3000) return false;

    return switch (cp) {
        0x9...0xd => true,
        0x20 => true,
        0x85 => true,
        0xa0 => true,
        0x1680 => true,
        0x2000...0x200a => true,
        0x2028 => true,
        0x2029 => true,
        0x202f => true,
        0x205f => true,
        0x3000 => true,
        else => false,
    };
}

pub fn indexOfCodePoint(str: u.Txt, scalar: u21) ?usize {
    var iter = @This(){ .cur = null, .bytes = str };
    while (iter.next()) |cp| {
        if (cp.scalar == scalar)
            return cp.offset;
    }
    return null;
}
