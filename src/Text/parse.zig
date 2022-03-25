const std = @import("std");
const u = @import("../util.zig");
const Expr = @import("../Expr.zig");

fn UN(comptime N: u16) type {
    return std.meta.Int(.unsigned, N);
}
fn SN(comptime N: u16) type {
    return std.meta.Int(.signed, N);
}
fn IN(comptime N: u16) type {
    return SN(N + 1);
}

inline fn digit(c: u8, hexnum: bool) !u8 {
    return switch (c) {
        '0'...'9' => c - '0',
        'A'...'F' => if (hexnum) c - 'A' + 10 else null,
        'a'...'f' => if (hexnum) c - 'a' + 10 else null,
        else => null,
    } orelse error.NotDigit;
}
fn iN(arg: Expr, comptime N: u16) !IN(N) {
    const str = arg.val.asKeyword() orelse return error.NotInt;
    var i: usize = 0;

    const sign = str[i] == '-';
    if (sign or str[i] == '+') i += 1;

    const hexnum = std.mem.startsWith(u8, str[i..], "0x");
    if (hexnum) i += 2;

    if (str.len <= i) return error.Empty;
    const V = IN(N);
    var v: V = try digit(str[i], hexnum);
    i += 1;

    while (i < str.len) : (i += 1) {
        if (str[i] == '_') {
            i += 1;
            if (str.len <= i) return error.Empty;
        }
        const d = try digit(str[i], hexnum);
        v = try std.math.mul(V, v, @as(u8, if (hexnum) 16 else 10));
        v = try std.math.add(V, v, d);
    }
    if (sign)
        v = try std.math.negate(v);

    return v;
}
fn uN(arg: Expr, comptime N: u16) !UN(N) {
    const i = try iN(arg, N);
    if (i < 0) return error.Signed;
    return std.math.lossyCast(UN(N), i);
}
fn sN(arg: Expr, comptime N: u16) !SN(N) {
    return iN(arg, N - 1);
}
pub fn u32_(arg: Expr) !u32 {
    return uN(arg, 32);
}
pub fn i32_(arg: Expr) !i32 {
    return sN(arg, 32);
}
pub fn i64_(arg: Expr) !i64 {
    return sN(arg, 64);
}
inline fn kv(str: u.Txt) Expr {
    return .{ .val = .{ .keyword = str } };
}
pub inline fn u32s(str: u.Txt) !u32 {
    return u32_(kv(str));
}
pub inline fn i32s(str: u.Txt) !i32 {
    return i32_(kv(str));
}
pub inline fn i64s(str: u.Txt) !i64 {
    return i64_(kv(str));
}

pub fn asFuncNamed(arg: Expr, comptime name: u.Txt) ?Expr.Val.Func {
    const func = arg.val.asFunc() orelse return null;
    return if (u.strEql(name, func.name)) func else null;
}
pub fn nTypeuse(args: []const Expr) usize {
    var i: usize = 0;
    if (args.len > 0 and asFuncNamed(args[0], "type") != null)
        i += 1;
    while (i < args.len) : (i += 1) {
        if (asFuncNamed(args[i], "param") == null) break;
    }
    while (i < args.len) : (i += 1) {
        if (asFuncNamed(args[i], "result") == null) break;
    }
    return i;
}
pub fn enumKv(comptime T: type) fn (arg: Expr) ?T {
    return struct {
        fn do(arg: Expr) ?T {
            const key = arg.val.asKeyword() orelse return null;
            return std.meta.stringToEnum(T, key);
        }
    }.do;
}
pub const numtype = enumKv(std.wasm.Valtype);
