const std = @import("std");
const u = @import("../util.zig");
const Expr = @import("../Expr.zig");
const IR = @import("../IR.zig");

const Opt = struct {
    ids: bool = false,
};
pub fn emit(m: IR.Module, allocator: std.mem.Allocator, comptime opt: Opt) !Expr {
    var n_top: usize = 1;
    n_top += m.funcs.len;
    n_top += @boolToInt(m.memory != null);
    n_top += m.datas.len;

    var top = try Arr.init(n_top, allocator);
    top.append(keyword("module"));

    for (m.funcs) |func| {
        var n_arr = 1 + func.exports.len;
        n_arr += @boolToInt(opt.ids and func.id != null);
        n_arr += @boolToInt(func.type.params.len > 0);
        n_arr += @boolToInt(func.type.results.len > 0);
        n_arr += switch (func.body) {
            .import => @as(usize, 1),
            .code => @panic("TODO: wasm2wat"),
        };

        var arr = try Arr.init(n_arr, allocator);
        arr.append(keyword("func"));

        if (opt.ids) if (func.id) |v|
            arr.append(try id(v, allocator));

        for (func.exports) |expor|
            arr.append(try exportAbrev(expor, allocator));
        switch (func.body) {
            .import => |impor| arr.append(try importAbbrev(impor, allocator)),
            else => {},
        }

        if (func.type.params.len > 0)
            arr.append(try valtypes("param", func.type.params, allocator));
        if (func.type.results.len > 0)
            arr.append(try valtypes("result", func.type.results, allocator));
        switch (func.body) {
            .import => {},
            .code => unreachable,
        }

        top.append(arr.finish());
    }

    if (m.memory) |mem| {
        var n_arr: usize = 2 + mem.exports.len;
        n_arr += @boolToInt(mem.size.max != null);
        n_arr += @boolToInt(mem.import != null);
        var arr = try Arr.init(n_arr, allocator);
        arr.append(keyword("memory"));

        for (mem.exports) |expor|
            arr.append(try exportAbrev(expor, allocator));
        if (mem.import) |impor|
            arr.append(try importAbbrev(impor, allocator));

        arr.append(try number(mem.size.min, allocator));
        if (mem.size.max) |max|
            arr.append(try number(max, allocator));

        top.append(arr.finish());
    }

    for (m.datas) |data| {
        var n_arr: usize = 1 + @as(usize, switch (data.body) {
            .active => @panic("TODO: wasm2wat"),
            .passive => 1,
        });

        var arr = try Arr.init(n_arr, allocator);
        arr.append(keyword("data"));

        switch (data.body) {
            .active => |act| {
                std.debug.assert(act.mem == 0);
                unreachable;
                // arr.append(act.offset);
                // arr.append(try string(act.content, allocator));
            },
            .passive => |pas| arr.append(try string(pas, allocator)),
        }

        top.append(arr.finish());
    }

    return top.finish();
}

inline fn keyword(name: u.Txt) Expr {
    return .{ .val = .{ .keyword = name } };
}
inline fn list(content: []Expr) Expr {
    return .{ .val = .{ .list = content } };
}
fn string(from: u.Txt, allocator: std.mem.Allocator) !Expr {
    return Expr{ .val = .{ .string = try allocator.dupe(u8, from) } };
}
fn id(from: u.Txt, allocator: std.mem.Allocator) !Expr {
    return Expr{ .val = .{ .id = try allocator.dupe(u8, from) } };
}
fn number(n: i64, allocator: std.mem.Allocator) !Expr {
    return keyword(try std.fmt.allocPrint(allocator, "{d}", .{n}));
}
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
    exp[0] = keyword("export");
    exp[1] = try string(expor, allocator);
    return list(exp);
}
fn importAbbrev(impor: IR.ImportName, allocator: std.mem.Allocator) !Expr {
    const imp = try allocator.alloc(Expr, 3);
    imp[0] = keyword("import");
    imp[1] = try string(impor.module, allocator);
    imp[2] = try string(impor.name, allocator);
    return list(imp);
}
fn valtypes(comptime key: u.Txt, of: []const IR.Sigtype, allocator: std.mem.Allocator) !Expr {
    const typ = try allocator.alloc(Expr, 1 + of.len);
    typ[0] = keyword(key);
    for (of) |param, i| {
        typ[i + 1] = keyword(@tagName(param.lower()));
    }
    return list(typ);
}
