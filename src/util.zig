const std = @import("std");

fn ByteSlice(comptime tag: []const u8) type {
    // probably not types checked
    // https://github.com/ziglang/zig/issues/1595
    _ = tag;
    return []const u8;
}

pub const Txt = ByteSlice("unicode");
pub const Bin = ByteSlice("binary");

pub const isTxt = std.unicode.utf8ValidateSlice;
pub inline fn toTxt(v: Bin) !Txt {
    if (!isTxt(v)) return error.InvalidUtf8;
    return v;
}

pub inline fn strEql(a: Txt, b: Txt) bool {
    return std.mem.eql(u8, a, b);
}
