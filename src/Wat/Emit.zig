const std = @import("std");
const u = @import("../util.zig");
const Expr = @import("../Expr.zig");
const IR = @import("../IR.zig");
const Wasm = @import("../Wasm.zig");

const Opt = struct {
    ids: bool = false,
};
pub fn emit(m: IR.Module, gpa: std.mem.Allocator, comptime opt: Opt) !Expr.Root {
    var n_top: usize = 1;
    n_top += m.funcs.len;
    n_top += @boolToInt(m.memory != null);
    n_top += m.datas.len;

    var arena = std.heap.ArenaAllocator.init(gpa);
    const allocator = arena.allocator();

    var top = try Arr.init(n_top, allocator);
    top.append(keyword("module"));

    for (m.funcs) |func| {
        var n_arr = 1 + func.exports.len;
        n_arr += @boolToInt(opt.ids and func.id != null);
        n_arr += @boolToInt(func.type.params.len > 0);
        n_arr += @boolToInt(func.type.results.len > 0);
        n_arr += switch (func.body) {
            .import => @as(usize, 1),
            .code => |code| blk: {
                var n: usize = 0;
                var reader = Wasm.Reader.init(code.bytes);
                const nLocals = try reader.uleb32();
                if (nLocals > 0) {
                    try reader.it.seekBy(nLocals * 2);
                    n += 1;
                }
                var depth: usize = 1;
                while (true) {
                    const inst = try reader.op(&depth);
                    if (depth == 0) break;
                    n += 1 + @as(usize, switch (inst.arg) {
                        .none => 0,
                        .int, .float, .idx => 1,
                        .table, .xy => 2,
                        .memarg => |arg| @as(usize, 1) + @boolToInt(arg.offset != 0),
                        .blocktype => |b| @boolToInt(b != .empty),
                    });
                }
                if (!reader.finished()) return error.NoFuncEnd;
                break :blk n;
            },
        };

        var arr = try Arr.init(n_arr, allocator);
        arr.append(keyword("func"));

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
            .code => |code| {
                //TODO: relocate
                var reader = Wasm.Reader.init(code.bytes);
                var nLocals = try reader.uleb32();
                if (nLocals > 0) {
                    var lcs = try Arr.init(2, allocator);
                    lcs.append(keyword("local"));
                    while (nLocals > 0) : (nLocals -= 1) {
                        const n = try reader.uleb32();
                        const typ = try reader.valtype();
                        try lcs.inner.appendNTimes(allocator, keyword(@tagName(typ)), n);
                    }
                    arr.append(lcs.finish());
                }
                var depth: usize = 1;
                while (true) {
                    const inst = try reader.op(&depth);
                    if (depth == 0) break;
                    arr.append(try opToKeyword(inst.op, allocator));
                    switch (inst.arg) {
                        .none => {},
                        .int => |i| arr.append(try int(i, allocator)),
                        .idx => |i| arr.append(try int(i, allocator)),
                        .float => |f| arr.append(try float(f, allocator)),
                        .memarg => |arg| {
                            if (arg.offset != 0)
                                arr.append(try prefixedInt(arg.offset, "offset=", allocator));
                            arr.append(try prefixedInt(@as(i64, 1) << @truncate(u6, arg.align_), "align=", allocator));
                        },
                        .blocktype => |b| switch (b) {
                            .empty => {},
                            .valtype => |v| {
                                var val = try Arr.init(2, allocator);
                                val.append(keyword("result"));
                                val.append(keyword(@tagName(v)));
                                arr.append(val.finish());
                            },
                            .idx => |v| {
                                std.debug.print("{}", .{v});
                                //FIXME: func types
                                unreachable;
                            },
                        },
                        .table => |t| {
                            var lst = try Arr.init(t.n, allocator);
                            var rd = Wasm.Reader.init(t.buf);
                            var n: usize = 0;
                            while (n < t.n) : (n += 1)
                                lst.append(try int(try rd.uleb32(), allocator));
                            arr.append(lst.finish());
                            arr.append(try int(try rd.uleb32(), allocator));
                            std.debug.assert(rd.finished());
                        },
                        .xy => |xy| for (xy) |v|
                            arr.append(try int(v, allocator)),
                    }
                }
                std.debug.assert(reader.finished());
            },
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
        arr.append(keyword("data"));

        const content = switch (data.body) {
            .active => |act| blk: {
                std.debug.assert(act.mem == 0);
                arr.append(try constExpr(act.offset, allocator));
                break :blk act.content;
            },
            .passive => |pas| pas,
        };
        arr.append(try string(content, allocator));

        top.append(arr.finish());
    }

    return Expr.Root{ .val = top.finish(), .arena = arena };
}

inline fn list(content: []Expr) Expr {
    return .{ .val = .{ .list = content } };
}
inline fn keyword(str: u.Txt) Expr {
    return .{ .val = .{ .keyword = str } };
}
fn StrExpr(comptime field: u.Txt) fn (u.Txt, std.mem.Allocator) std.mem.Allocator.Error!Expr {
    return struct {
        fn f(str: u.Txt, allocator: std.mem.Allocator) !Expr {
            return Expr{ .val = @unionInit(Expr.Val, field, try allocator.dupe(u8, str)) };
        }
    }.f;
}
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
fn prefixedInt(v: i64, comptime prefix: u.Txt, allocator: std.mem.Allocator) !Expr {
    //MAYBE: deduplicated
    var buf: [prefix.len + 20]u8 = undefined;
    std.mem.copy(u8, &buf, prefix);
    const len = std.fmt.formatIntBuf(buf[prefix.len..], v, 10, .lower, .{});
    return StrExpr("keyword")(buf[0 .. prefix.len + len], allocator);
}
fn int(v: i64, allocator: std.mem.Allocator) !Expr {
    return prefixedInt(v, "", allocator);
}
fn float(v: f64, allocator: std.mem.Allocator) !Expr {
    //FIXME: check for spec compliance
    var counter = std.io.countingWriter(std.io.null_writer);
    try std.fmt.formatFloatScientific(v, .{}, counter.writer());
    const buf = try allocator.alloc(u8, counter.bytes_written);
    var fixed = std.io.fixedBufferStream(buf);
    try std.fmt.formatFloatScientific(v, .{}, fixed.writer());
    return keyword(fixed.getWritten());
}
fn constExpr(expr: IR.InitExpr, allocator: std.mem.Allocator) !Expr {
    var arr = try Arr.init(2, allocator);
    arr.append(try opToKeyword(IR.initExpr(expr), allocator));
    arr.append(switch (expr) {
        .i32_const => |i| try int(i, allocator),
        .i64_const => |i| try int(i, allocator),
        .f32_const => |f| try float(f, allocator),
        .f64_const => |f| try float(f, allocator),
        .global_get => |i| try int(i, allocator),
    });
    return arr.finish();
}

inline fn opToKeyword(op: IR.Code.Op, allocator: std.mem.Allocator) std.mem.Allocator.Error!Expr {
    const tag = @tagName(op);
    if (std.mem.indexOfScalar(u8, tag, '_')) |i| if (!std.mem.startsWith(u8, tag, "br")) {
        var buf: [32]u8 = undefined;
        const slice = buf[0..tag.len];
        std.mem.copy(u8, slice, tag);
        slice[i] = '.';
        return StrExpr("keyword")(slice, allocator);
    };
    return keyword(tag);
}
