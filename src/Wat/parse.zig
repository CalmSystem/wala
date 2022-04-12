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
fn num(comptime V: type, str: u.Txt, hexnum: bool) !V {
    if (str.len == 0) return error.Empty;
    var v: V = try digit(str[0], hexnum);

    var i: usize = 1;
    while (i < str.len) : (i += 1) {
        if (str[i] == '_') {
            i += 1;
            if (str.len <= i) return error.Empty;
        }
        const d = try digit(str[i], hexnum);
        v = try std.math.mul(V, v, @as(u8, if (hexnum) 16 else 10));
        v = try std.math.add(V, v, d);
    }
    return v;
}
fn iN(str: u.Txt, comptime N: u16) !IN(N) {
    var i: usize = 0;

    if (str.len == 0) return error.Empty;
    const sign = str[i] == '-';
    if (sign or str[i] == '+') i += 1;

    const hexnum = std.mem.startsWith(u8, str[i..], "0x");
    if (hexnum) i += 2;

    var v = try num(IN(N), str[i..], hexnum);
    if (sign)
        v = try std.math.negate(v);

    return v;
}
fn uN(str: u.Txt, comptime N: u16) !UN(N) {
    const i = try iN(str, N);
    if (i < 0) return error.Signed;
    return std.math.lossyCast(UN(N), i);
}
fn sN(str: u.Txt, comptime N: u16) !SN(N) {
    return iN(str, N - 1);
}
pub fn fNs(str: u.Txt) !f64 {
    var i: usize = 0;

    if (str.len == 0) return error.Empty;
    const sign = str[i] == '-';
    if (sign or str[i] == '+') i += 1;

    var v = if (u.strEql("inf", str[i..]))
        std.math.inf_f64
    else if (std.mem.startsWith(u8, str[i..], "nan")) blk: {
        // MAYBE: support nan:0x hexnum
        if (str[i..].len > 3) return error.TooMany;
        break :blk std.math.nan_f64;
    } else blk: {
        // NOTE: not correct on i64 overflow
        const hexnum = std.mem.startsWith(u8, str[i..], "0x");
        if (hexnum) i += 2;

        const dot = std.mem.indexOfScalar(u8, str[i..], '.');
        var p = @intToFloat(f64, try num(i64, if (dot) |j| str[i .. i + j] else str[i..], hexnum));
        if (dot == null or i + dot.? + 1 >= str.len) break :blk p;

        i += dot.? + 1;
        const pow = std.mem.indexOfScalar(u8, str[i..], 'p') orelse std.mem.indexOfScalar(u8, str[i..], 'P');
        if (pow == null or str.len > pow.? + 1) {
            const q = @intToFloat(f64, try num(i64, if (pow) |j| str[i .. i + j] else str[i..], hexnum));
            p *= (1.0 / q);
        }

        if (pow != null) { //exp
            i += pow.? + 1;
            if (str.len >= i) return error.Empty;
            const exp_sign = str[i] == '-';
            if (exp_sign or str[i] == '+') i += 1;
            var e = @intToFloat(f64, try num(i64, str[i..], false));
            if (exp_sign)
                e *= -1;
            p *= std.math.pow(f64, 2, e);
        }
        break :blk p;
    };
    if (sign)
        v *= -1;

    return v;
}

pub fn keyword(arg: Expr) !u.Txt {
    return arg.val.asKeyword() orelse return error.NotKeyword;
}
pub fn u32s(str: u.Txt) !u32 {
    return uN(str, 32);
}
pub fn i32s(str: u.Txt) !i32 {
    return sN(str, 32);
}
pub fn i64s(str: u.Txt) !i64 {
    return sN(str, 64);
}
pub inline fn u32_(arg: Expr) !u32 {
    return u32s(try keyword(arg));
}

pub const Func = struct {
    name: u.Txt,
    id: ?u.Txt = null,
    args: []Expr = &[_]Expr{},
};
pub fn asFunc(arg: Expr) ?Func {
    switch (arg.val) {
        .list => |exprs| {
            if (exprs.len > 0) {
                if (exprs[0].val.asKeyword()) |name| {
                    const id = if (exprs.len > 1) exprs[1].val.asId() else null;
                    const offset = @as(usize, 1) + @boolToInt(id != null);
                    return Func{ .name = name, .id = id, .args = exprs[offset..] };
                }
            }
        },
        else => {},
    }
    return null;
}
pub fn asFuncNamed(arg: Expr, comptime name: u.Txt) ?Func {
    const func = asFunc(arg) orelse return null;
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
