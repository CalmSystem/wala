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

pub inline fn constSliceExpand(comptime T: type, allocator: std.mem.Allocator, slice: *[]const T, n: usize) ![]T {
    const resized = try allocator.realloc(@intToPtr(*[]T, @ptrToInt(slice)).*, slice.len + n);
    slice.* = resized;
    return resized[resized.len - n ..];
}

pub const ResultKind = enum { ok, err };
pub fn Result(comptime ok: type, comptime err: type) type {
    return union(ResultKind) {
        ok: ok,
        err: err,
    };
}
