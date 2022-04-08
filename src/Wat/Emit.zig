const std = @import("std");
const u = @import("../util.zig");
const Expr = @import("../Expr.zig");
const IR = @import("../IR.zig");
const Wasm = @import("../Wasm.zig");

const Opt = struct {
    ids: bool = false,
};
pub fn emit(m: IR.Module, allocator: std.mem.Allocator, comptime opt: Opt) !Expr {
    var n_top: usize = 1;
    n_top += m.funcs.len;
    n_top += @boolToInt(m.memory != null);
    n_top += m.datas.len;

    var top = try Arr.init(n_top, allocator);
    top.append(try keyword("module", allocator));

    for (m.funcs) |func| {
        var n_arr = 1 + func.exports.len;
        n_arr += @boolToInt(opt.ids and func.id != null);
        n_arr += @boolToInt(func.type.params.len > 0);
        n_arr += @boolToInt(func.type.results.len > 0);
        n_arr += switch (func.body) {
            .import => @as(usize, 1),
            .code => |code| try sizeCode(code, true),
        };

        var arr = try Arr.init(n_arr, allocator);
        arr.append(try keyword("func", allocator));

        if (opt.ids) if (func.id) |v|
            arr.append(try id(v, allocator));

        for (func.exports) |expor|
            arr.append(try exportAbrev(expor, allocator));
        switch (func.body) {
            .import => |impor| arr.append(try importAbbrev(impor, allocator)),
            .code => {},
        }

        if (func.type.params.len > 0)
            arr.append(try valtypes("param", func.type.params, allocator));
        if (func.type.results.len > 0)
            arr.append(try valtypes("result", func.type.results, allocator));
        switch (func.body) {
            .import => {},
            .code => |code| try emitCode(code, &arr, allocator, true),
        }

        top.append(arr.finish());
    }

    if (m.memory) |mem| {
        var n_arr: usize = 2 + mem.exports.len;
        n_arr += @boolToInt(mem.size.max != null);
        n_arr += @boolToInt(mem.import != null);
        var arr = try Arr.init(n_arr, allocator);
        arr.append(try keyword("memory", allocator));

        for (mem.exports) |expor|
            arr.append(try exportAbrev(expor, allocator));
        if (mem.import) |impor|
            arr.append(try importAbbrev(impor, allocator));

        arr.append(try int(mem.size.min, allocator));
        if (mem.size.max) |max|
            arr.append(try int(max, allocator));

        top.append(arr.finish());
    }

    for (m.datas) |data| {
        const n_arr = @as(usize, switch (data.body) {
            .active => 2,
            .passive => 1,
        }) + 1;

        var arr = try Arr.init(n_arr, allocator);
        arr.append(try keyword("data", allocator));

        const content = switch (data.body) {
            .active => |act| blk: {
                std.debug.assert(act.mem == 0);
                var offset = try Arr.init(try sizeCode(act.offset, false), allocator);
                try emitCode(act.offset, &offset, allocator, false);
                arr.append(offset.finish());
                break :blk act.content;
            },
            .passive => |pas| pas,
        };
        arr.append(try string(content, allocator));

        top.append(arr.finish());
    }

    return top.finish();
}

inline fn list(content: []Expr) Expr {
    return .{ .val = .{ .list = content } };
}
fn StrExpr(comptime field: u.Txt) fn (u.Txt, std.mem.Allocator) std.mem.Allocator.Error!Expr {
    return struct {
        fn f(str: u.Txt, allocator: std.mem.Allocator) !Expr {
            return Expr{ .val = @unionInit(Expr.Val, field, try allocator.dupe(u8, str)) };
        }
    }.f;
}
const keyword = StrExpr("keyword");
const string = StrExpr("string");
const id = StrExpr("id");

const Arr = struct {
    inner: std.ArrayListUnmanaged(Expr),

    fn init(size: usize, allocator: std.mem.Allocator) !Arr {
        return Arr{ .inner = try std.ArrayListUnmanaged(Expr).initCapacity(allocator, size) };
    }
    fn appendSlice(self: *Arr, exprs: []const Expr) void {
        self.inner.appendSliceAssumeCapacity(exprs);
    }
    fn append(self: *Arr, expr: Expr) void {
        self.inner.appendAssumeCapacity(expr);
    }
    inline fn finish(self: Arr) Expr {
        return list(self.inner.items);
    }
};

fn exportAbrev(expor: IR.ExportName, allocator: std.mem.Allocator) !Expr {
    const exp = try allocator.alloc(Expr, 2);
    exp[0] = try keyword("export", allocator);
    exp[1] = try string(expor, allocator);
    return list(exp);
}
fn importAbbrev(impor: IR.ImportName, allocator: std.mem.Allocator) !Expr {
    const imp = try allocator.alloc(Expr, 3);
    imp[0] = try keyword("import", allocator);
    imp[1] = try string(impor.module, allocator);
    imp[2] = try string(impor.name, allocator);
    return list(imp);
}
fn valtypes(comptime key: u.Txt, of: []const IR.Sigtype, allocator: std.mem.Allocator) !Expr {
    const typ = try allocator.alloc(Expr, 1 + of.len);
    typ[0] = try keyword(key, allocator);
    for (of) |param, i| {
        typ[i + 1] = try keyword(@tagName(param.lower()), allocator);
    }
    return list(typ);
}
fn prefixedInt(v: i64, comptime prefix: u.Txt, allocator: std.mem.Allocator) !Expr {
    //MAYBE: deduplicated
    var buf: [prefix.len + 20]u8 = undefined;
    std.mem.copy(u8, &buf, prefix);
    const len = std.fmt.formatIntBuf(buf[prefix.len..], v, 10, .lower, .{});
    return keyword(buf[0 .. prefix.len + len], allocator);
}
fn int(v: i64, allocator: std.mem.Allocator) !Expr {
    return prefixedInt(v, "", allocator);
}

fn sizeCode(code: IR.Code, hasLocals: bool) !usize {
    var n: usize = 0;
    var reader = Wasm.CodeReader.init(code.bytes);
    if (hasLocals) {
        const nLocals = try reader.uleb32();
        if (nLocals > 0) {
            try reader.it.seekBy(nLocals * 2);
            n += 1;
        }
    }
    while (try reader.next()) |inst| {
        n += 1 + @as(usize, switch (inst.arg) {
            .none => 0,
            .int, .float, .idx => 1,
            .memarg => |m| @as(usize, 1) + @boolToInt(m.offset != 0),
            .blocktype => |b| @boolToInt(b != .empty),
        });
    }
    return n;
}
inline fn opToKeyword(op: IR.Code.Op, allocator: std.mem.Allocator) std.mem.Allocator.Error!Expr {
    const tag = @tagName(op);
    if (std.mem.indexOfScalar(u8, tag, '_')) |i| if (!std.mem.startsWith(u8, tag, "br")) {
        var buf: [32]u8 = undefined;
        const slice = buf[0..tag.len];
        std.mem.copy(u8, slice, tag);
        slice[i] = '.';
        return keyword(slice, allocator);
    };
    return keyword(tag, allocator);
}
fn emitCode(code: IR.Code, out: *Arr, allocator: std.mem.Allocator, hasLocals: bool) !void {
    //TODO: relocate
    var reader = Wasm.CodeReader.init(code.bytes);
    if (hasLocals) {
        var nLocals = try reader.uleb32();
        if (nLocals > 0) {
            var arr = try Arr.init(2, allocator);
            arr.append(try keyword("local", allocator));
            while (nLocals > 0) : (nLocals -= 1) {
                const n = try reader.uleb32();
                const typ = try reader.valtype();
                try arr.inner.appendNTimes(allocator, try keyword(@tagName(typ), allocator), n);
            }
            out.append(arr.finish());
        }
    }
    while (try reader.next()) |inst| {
        out.append(try opToKeyword(inst.op, allocator));
        switch (inst.arg) {
            .none => {},
            .int => |i| out.append(try int(i, allocator)),
            .idx => |i| out.append(try int(i, allocator)),
            .float => {
                //FIXME: float to string
                unreachable;
            },
            .memarg => |m| {
                if (m.offset != 0)
                    out.append(try prefixedInt(m.offset, "offset=", allocator));
                out.append(try prefixedInt(@as(i64, 1) << @truncate(u6, m.align_), "align=", allocator));
            },
            .blocktype => |b| switch (b) {
                .empty => {},
                .valtype => |v| {
                    var arr = try Arr.init(2, allocator);
                    arr.append(try keyword("result", allocator));
                    arr.append(try keyword(@tagName(v), allocator));
                    out.append(arr.finish());
                },
                .idx => |v| {
                    std.debug.print("{}", .{v});
                    //FIXME: func types
                    unreachable;
                },
            },
        }
    }
}
