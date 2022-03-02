const u = @import("util.zig");
const ziglyph = @import("ziglyph");
pub const Inner = ziglyph.CodePointIterator;

cp_iter: Inner,
cur: ?ziglyph.CodePoint,

const Self = @This();
const eofBytes = &[_]u8{};

pub inline fn init(bytes: u.Bin) !Self {
    return unsafeInit(try u.toTxt(bytes));
}
pub fn unsafeInit(str: u.Txt) Self {
    var iter = Inner{ .bytes = str };
    return Self{ .cur = iter.next(), .cp_iter = iter };
}

pub fn next(self: *Self) ?ziglyph.CodePoint {
    self.cur = self.cp_iter.next();
    return self.cur;
}
pub fn skip(self: *Self) void {
    _ = self.next();
}
pub fn peek(self: Self) ziglyph.CodePoint {
    return if (self.cur) |c| c else ziglyph.CodePoint{ .scalar = 0, .offset = self.cp_iter.bytes.len, .bytes = eofBytes };
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
            self.cp_iter.bytes[p.offset..q.offset]
        else
            self.cp_iter.bytes[p.offset..];
    } else return eofBytes;
}

pub const isSpace = ziglyph.isWhiteSpace;

pub fn indexOfCodePoint(str: u.Txt, scalar: u21) ?usize {
    var iter = Inner{ .bytes = str };
    while (iter.next()) |cp| {
        if (cp.scalar == scalar)
            return cp.offset;
    }
    return null;
}
