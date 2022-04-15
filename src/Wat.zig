const u = @import("util.zig");
const std = @import("std");
const Expr = @import("Expr.zig");
const IR = @import("IR.zig");
const p = @import("Wat/parse.zig");
const Codegen = @import("Wat/Codegen.zig");
pub const emit = @import("Wat/Emit.zig").emit;

const TopDefinition = enum { type, import, func, data, @"export", memory, table, elem, global };

/// child_allocator is used as gpa()
arena: std.heap.ArenaAllocator,
/// Work in progress IR.Module
I: struct {
    // (type (func ...))
    funcTypes: UnmanagedIndex(IR.Func.Sig) = .{},
    funcs: UnmanagedIndex(FuncProgress) = .{},
    funcImportCount: usize = 0,
    datas: UnmanagedIndex(union(enum) {
        args: []const Expr,
        done: IR.Data,
    }) = .{},
    globals: UnmanagedIndex(void) = .{},
    exports: std.ArrayListUnmanaged([]const Expr) = .{},

    memory: ?IR.Memory = null,
} = .{},
/// Parsing pointer
at: ?Expr = null,
err: ErrData = null,

const Ctx = @This();

pub const ErrData = ?union(enum) {
    typeMismatch: struct {
        expect: ?Codegen.Stack,
        got: ?Codegen.Stack,
        index: ?usize = null,
    },

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .typeMismatch => |m| {
                try writer.print("Expected {} got {}", .{ m.expect, m.got });
                if (m.index) |i| try writer.print(" at index {}", .{i});
            },
        }
    }
};
const Result = u.Result(IR.Module, struct {
        kind: anyerror,
        data: ErrData = null,
        at: ?Expr = null,
});
pub fn tryLoad(exprs: []const Expr, allocator: std.mem.Allocator, loader: anytype) Result {
    //TODO: load recussive with (use ?id name)
    _ = loader.load;

    var module: ?IR.Module = null;
    for (exprs) |expr| {
        const func = p.asFuncNamed(expr, "module") orelse continue;
        const child = switch (tryLoadModule(func.args, allocator)) {
            .ok => |m| m,
            .err => |err| return .{ .err = err },
        };
        if (module) |parent| {
            module = IR.Module.link(&[_]IR.Module{ parent, child }, allocator) catch |err|
                return .{ .err = .{ .kind = err } };
            parent.deinit();
        } else {
            module = child;
        }
    }

    return if (module) |m| .{ .ok = m } else // without top module
    tryLoadModule(exprs, allocator);
}
fn tryLoadModule(exprs: []const Expr, allocator: std.mem.Allocator) Result {
    var ctx = Ctx{ .arena = std.heap.ArenaAllocator.init(allocator) };
    const module = ctx._loadModule(exprs) catch |err|
        return .{ .err = .{ .kind = err, .data = ctx.err, .at = ctx.at } };
    return .{ .ok = module };
}
fn _loadModule(ctx: *Ctx, exprs: []const Expr) !IR.Module {
    // name = if (exprs[0] dollared) ident else "env"

    for (exprs) |expr|
        try ctx.buildIndex(expr);

    const datas = try ctx.m_allocator().alloc(IR.Data, ctx.I.datas.items.len);
    for (ctx.I.datas.items) |*data, i| {
        const done = try ctx.loadData(data.val.args, data.id);
        data.val = .{ .done = done };
        datas[i] = done;
    }

    for (ctx.I.exports.items) |expr|
        try ctx.loadExport(expr);

    const funcs = try ctx.m_allocator().alloc(IR.Func, ctx.I.funcs.items.len);
    for (ctx.I.funcs.items) |*f, i| {
        if (i > ctx.I.funcImportCount and f.val == .unused and p.asFuncNamed(f.val.unused.args[0], "export") == null)
            std.log.warn("Unused func {}({s})", .{ i, f.id });
        try ctx.loadFunc(&f.val, f.id);
        funcs[i] = f.val.done;
    }

    ctx.I.funcTypes.deinit(ctx.gpa());
    ctx.I.funcs.deinit(ctx.gpa());
    ctx.I.datas.deinit(ctx.gpa());
    ctx.I.globals.deinit(ctx.gpa());
    ctx.I.exports.deinit(ctx.gpa());

    return IR.Module{
        .arena = ctx.arena,

        .funcs = funcs,
        .tables = &[_]IR.Table{},
        .memory = ctx.I.memory,
        .globals = &[_]IR.Global{},
        .start = null,
        .elements = &[_]IR.Elem{},
        .datas = datas,

        //.linking = Linking,
        .customs = &[_]IR.Section.Custom{},
    };
}
inline fn buildIndex(ctx: *Ctx, expr: Expr) !void {
    const func = p.asFunc(expr) orelse return error.NotFuncTopValue;
    const section = std.meta.stringToEnum(TopDefinition, func.name) orelse return error.UnknownSection;
    if (func.args.len == 0) return error.Empty;

    ctx.at = expr;
    switch (section) {
        .type => try ctx.buildType(func),
        .func => try ctx.buildFunc(func, null),
        .table => @panic("TODO:"),
        .memory => try ctx.buildMemory(func, null),
        .global => @panic("TODO:"),
        .@"export" => try ctx.I.exports.append(ctx.gpa(), func.args),
        .elem => @panic("TODO:"),
        .data => {
            try indexFreeId(ctx.I.datas, func.id);
            try ctx.I.datas.append(ctx.gpa(), .{ .id = try ctx.dupeN(func.id), .val = .{ .args = func.args } });
        },
        .import => try ctx.buildImport(func.args),
    }
}

inline fn buildType(ctx: *Ctx, func: p.Func) !void {
    if (func.args.len > 1) return error.TooMany;
    const inner = p.asFuncNamed(func.args[0], "func") orelse return error.NotFunc;
    const use = try ctx.typedef(inner.args);
    use.deinit(ctx);
    if (use.remain.len > 0) return error.TooMany;
    try indexFreeId(ctx.I.funcTypes, func.id);
    try ctx.I.funcTypes.append(ctx.gpa(), .{ .id = func.id, .val = use.val });
}
const FuncProgress = union(enum) {
    unused: struct {
        args: []const Expr,
        importParent: ?IR.ImportName,
    },
    typed: struct {
        it: IR.Func,
        insts: []const Expr,
        params: []const ?u.Txt,
        wip: bool = false,
    },
    done: IR.Func,
};
fn buildFunc(ctx: *Ctx, func: p.Func, importParent: ?IR.ImportName) !void {
    const importAbbrev = try hasImportAbbrev(func.args, importParent != null);
    try indexFreeId(ctx.I.funcs, func.id);
    const f = IndexField(FuncProgress){ .id = try ctx.dupeN(func.id), .val = .{ .unused = .{ .args = func.args, .importParent = importParent } } };
    if (importAbbrev or importParent != null) {
        try ctx.I.funcs.insert(ctx.gpa(), ctx.I.funcImportCount, f);
        ctx.I.funcImportCount += 1;
    } else {
        try ctx.I.funcs.append(ctx.gpa(), f);
    }
}
fn buildMemory(ctx: *Ctx, func: p.Func, importParent: ?IR.ImportName) !void {
    if (ctx.I.memory != null) return error.MultipleMemory;

    const importAbbrev = try hasImportAbbrev(func.args, importParent != null);
    const io = try ctx.exportsImportNames(func.args, importAbbrev, func.id);

    //TODO: data abbrev
    const memtyp = try limit(io.remain);

    ctx.I.memory = .{
        .id = try ctx.dupeN(func.id),
        .exports = io.exports,
        .import = importParent orelse io.import,
        .size = memtyp,
    };
}
inline fn buildImport(ctx: *Ctx, args: []const Expr) !void {
    if (args.len != 3) return error.Empty;
    const name = try ctx.importName(args[0..2]);

    ctx.at = args[2];
    const func = p.asFunc(args[2]) orelse return error.BadImport;
    const kind = std.meta.stringToEnum(TopDefinition, func.name) orelse return error.BadImport;

    switch (kind) {
        .func => try ctx.buildFunc(func, name),
        .table => @panic("TODO:"),
        .memory => try ctx.buildMemory(func, name),
        .global => @panic("TODO:"),
        else => return error.BadImport,
    }
}

pub fn typeFunc(ctx: *Ctx, fp: *FuncProgress, id: ?u.Txt) !*IR.Func {
    return switch (fp.*) {
        .done => &fp.done,
        .typed => &fp.typed.it,
        .unused => |f| {
            if (f.args.len > 0) ctx.at = f.args[0];
            const importAbbrev = try hasImportAbbrev(f.args, f.importParent != null);
            const io = try ctx.exportsImportNames(f.args, importAbbrev, id);

            var typ = try ctx.typeuse(io.remain);

            if (f.importParent orelse io.import) |import| {
                typ.deinit(ctx);
                if (typ.remain.len > 0)
                    return error.ImportWithBody;

                fp.* = .{ .done = .{
                    .body = .{ .import = import },
                    .id = id,
                    .exports = io.exports,
                    .type = typ.val,
                } };
                return &fp.done;
            } else {
                fp.* = .{ .typed = .{ .it = .{
                    .body = .{ .code = .{ .bytes = "" } },
                    .id = id,
                    .exports = io.exports,
                    .type = typ.val,
                }, .insts = typ.remain, .params = typ.params } };
                return &fp.typed.it;
            }
        },
    };
}
fn loadFunc(ctx: *Ctx, fp: *FuncProgress, id: ?u.Txt) !void {
    switch (fp.*) {
        .done => return,
        .typed => {},
        .unused => {
            _ = try ctx.typeFunc(fp, id);
            if (fp.* == .done) return;
        },
    }
    const t = &fp.typed;
    if (t.wip) return error.DependencyLoop;
    t.wip = true;

    if (t.insts.len > 0) ctx.at = t.insts[0];
    // locals
    const locals = try ctx.valtypesAlloc(t.insts, "local");

    var local_ids = try u.constSliceExpand(?u.Txt, ctx.gpa(), &t.params, locals.types.len);
    defer ctx.gpa().free(t.params);

    const insts = try ctx.valtypesRead(t.insts, "local", locals.types, local_ids);

    const code = try Codegen.load(ctx, insts, t.it.type, locals.types, t.params);
    t.it.body = .{ .code = code };
    fp.* = .{ .done = t.it };
}
inline fn loadExport(ctx: *Ctx, args: []const Expr) !void {
    if (args.len != 2) return error.Empty;
    const name = try ctx.string(args[0]);

    ctx.at = args[1];
    const func = p.asFunc(args[1]) orelse return error.BadExport;
    const kind = std.meta.stringToEnum(TopDefinition, func.name) orelse return error.BadExport;

    const exports: *[]const IR.ExportName = switch (kind) {
        .func => blk: {
            const i = try indexFindByF(ctx.I.funcs, func);
            const fp = &ctx.I.funcs.items[i];
            const f = try ctx.typeFunc(&fp.val, fp.id);
            break :blk &f.exports;
        },
        .table => @panic("TODO:"),
        .memory => blk: {
            const m = ctx.I.memory orelse return error.NotFound;
            if (func.id) |id| {
                if (m.id == null or !u.strEql(id, m.id.?))
                    return error.NotFound;
            } else if (func.args.len != 1 or (try p.u32_(func.args[0])) != 0)
                return error.NotFound;
            break :blk &ctx.I.memory.?.exports;
        },
        .global => @panic("TODO:"),
        else => return error.BadExport,
    };

    const new = try u.constSliceExpand(IR.ExportName, ctx.m_allocator(), exports, 1);
    new[0] = name;
}
fn loadData(ctx: *Ctx, args: []const Expr, id: ?u.Txt) !IR.Data {
    var d = IR.Data{ .id = id, .body = .{ .passive = "" } };

    var i: usize = 0;
    if (args.len > 0) {
        if (p.asFunc(args[0])) |first| { // active
            if (u.strEql("memory", first.name)) {
                //TODO: read mem id
                if (args.len < 2) return error.Empty;
                i += 1;
            }
            const exprs = if (p.asFuncNamed(args[i], "offset")) |func|
                func.args
            else
                args[i .. i + 1];

            const typ = IR.Func.Sig{ .results = &[_]IR.Sigtype{.i32} };
            const offset = try Codegen.load(ctx, exprs, typ, null, null);
            d.body = .{ .active = .{
                .mem = 0,
                .content = "",
                .offset = offset,
            } };
            i += 1;
        }
    }

    var len: usize = 0;
    for (args[i..]) |arg| {
        const str = arg.val.asString() orelse return error.NotString;
        len += str.len;
    }
    const datastring = try ctx.m_allocator().alloc(u8, len);
    len = 0;
    for (args[i..]) |arg| {
        const str = arg.val.asString() orelse unreachable;
        std.mem.copy(u8, datastring[len..], str);
        len += str.len;
    }

    switch (d.body) {
        .active => |*act| act.content = datastring,
        .passive => |*pas| pas.* = datastring,
    }

    return d;
}

fn IndexField(comptime Val: type) type {
    return struct { id: ?u.Txt, val: Val };
}
fn UnmanagedIndex(comptime Val: type) type {
    return std.ArrayListUnmanaged(IndexField(Val));
}
inline fn indexFreeId(idx: anytype, may_id: ?u.Txt) !void {
    if (indexFindById(idx, may_id orelse return) != null) return error.DuplicatedId;
}
pub fn indexFindById(idx: anytype, id: u.Txt) ?u32 {
    for (idx.items) |item, i|
        if (item.id) |other| if (u.strEql(id, other))
            return @truncate(u32, i);
    return null;
}
inline fn indexFindByF(idx: anytype, f: p.Func) !u32 {
    if (f.id) |id| {
        return indexFindById(idx, id) orelse error.NotFound;
    } else {
        if (f.args.len != 1) return error.Empty;
        const i = try p.u32_(f.args[0]);
        if (i >= idx.items.len) return error.NotFound;
        return i;
    }
}

fn limit(args: []const Expr) !std.wasm.Limits {
    return switch (args.len) {
        0 => error.Empty,
        1 => .{ .min = try p.u32_(args[0]), .max = null },
        2 => .{ .min = try p.u32_(args[0]), .max = try p.u32_(args[1]) },
        else => error.TooMany,
    };
}
fn hasImportAbbrev(args: []const Expr, importParent: bool) !bool {
    for (args) |arg| {
        if (p.asFuncNamed(arg, "import") != null) {
            if (importParent) return error.MultipleImport;
            return true;
        }
    }
    return false;
}
fn string(ctx: *Ctx, arg: Expr) !u.Bin {
    const str = arg.val.asString() orelse return error.NotString;
    return try ctx.m_allocator().dupe(u8, str);
}
inline fn dupeN(ctx: *Ctx, str: ?u.Bin) !?u.Bin {
    return if (str) |v| try ctx.m_allocator().dupe(u8, v) else null;
}
fn importName(ctx: *Ctx, args: []const Expr) !IR.ImportName {
    if (args.len < 2) return error.Empty;
    if (args.len > 2) return error.TooMany;
    return IR.ImportName{
        .module = try ctx.string(args[0]),
        .name = try ctx.string(args[1]),
    };
}
const ExportsImportName = struct {
    exports: []const IR.ExportName,
    import: ?IR.ImportName,
    remain: []const Expr,
};
fn exportsImportNames(ctx: *Ctx, args: []const Expr, withImport: bool, fallbackExport: ?u.Txt) !ExportsImportName {
    var i: usize = 0;
    while (i < args.len and p.asFuncNamed(args[i], "export") != null) : (i += 1) {}

    const exports = try ctx.m_allocator().alloc(IR.ExportName, i);
    i = 0;
    while (i < args.len) : (i += 1) {
        const expor = p.asFuncNamed(args[i], "export") orelse break;
        ctx.at = args[i];
        exports[i] = switch (expor.args.len) {
            1 => try ctx.string(expor.args[0]),
            0 => fallbackExport orelse return error.Empty,
            else => return error.TooMany,
        };
    }

    var import: ?IR.ImportName = null;
    if (withImport) {
        ctx.at = args[i];
        const impor = p.asFuncNamed(args[i], "import") orelse return error.BadImport;
        import = try ctx.importName(impor.args);
        i += 1;
    }
    return ExportsImportName{ .exports = exports, .import = import, .remain = args[i..] };
}
const TypeUse = struct {
    val: IR.Func.Sig,
    params: []?u.Txt,
    remain: []const Expr,

    pub inline fn deinit(self: TypeUse, ctx: *Ctx) void {
        ctx.gpa().free(self.params);
    }
};
fn ValtypesIter(comptime name: u.Txt) type {
    return struct {
        i: usize = 0,
        j: usize = 0,
        args: []const Expr,

        fn next(it: *@This()) !?Ret {
            while (it.i < it.args.len) : (it.i += 1) {
                const param = p.asFuncNamed(it.args[it.i], name) orelse break;
                if (it.j < param.args.len) {
                    const id = if (it.j == 0) param.id else null;
                    //TODO: abbrev ident
                    const v = p.enumKv(IR.Sigtype)(param.args[it.j]) orelse return error.NotValtype;
                    it.j += 1;
                    return Ret{ .id = id, .v = v };
                }
                it.j = 0;
            }
            return null;
        }
        const Ret = struct {
            id: ?u.Txt,
            v: IR.Sigtype,
        };
    };
}
const AllocVatypes = struct {
    types: []IR.Sigtype,
    remain: []const Expr,
};
fn valtypesAlloc(ctx: *Ctx, args: []const Expr, comptime name: u.Txt) !AllocVatypes {
    var it = ValtypesIter(name){ .args = args };
    var n: usize = 0;
    while ((try it.next()) != null) n += 1;
    return AllocVatypes{ .types = try ctx.m_allocator().alloc(IR.Sigtype, n), .remain = args[it.i..] };
}
fn valtypesRead(_: *Ctx, args: []const Expr, comptime name: u.Txt, types: []IR.Sigtype, ids: ?[]?u.Txt) ![]const Expr {
    var it = ValtypesIter(name){ .args = args };
    var n: usize = 0;
    while (try it.next()) |v| {
        if (ids) |slice|
            slice[n] = v.id;
        types[n] = v.v;
        n += 1;
    }
    std.debug.assert(n == types.len);
    return args[it.i..];
}
fn valtypesCheck(self: *Ctx, args: []const Expr, comptime name: u.Txt, types: []const IR.Sigtype, ids: ?[]?u.Txt) ![]const Expr {
    var it = ValtypesIter(name){ .args = args };
    var n: usize = 0;
    while (try it.next()) |v| {
        if (ids) |slice|
            slice[n] = v.id;

        if (!types[n].eql(v.v)) {
            self.err = .{ .typeMismatch = .{
                .expect = .{ .val = types[n] },
                .got = .{ .val = v.v },
                .index = n,
            } };
            return error.TypeMismatch;
        }
        n += 1;
    }
    return args[it.i..];
}
fn typedef(ctx: *Ctx, args: []const Expr) !TypeUse {
    const ps = try ctx.valtypesAlloc(args, "param");
    const rs = try ctx.valtypesAlloc(ps.remain, "result");

    const params = try ctx.gpa().alloc(?u.Txt, ps.types.len);

    var remain = try ctx.valtypesRead(args, "param", ps.types, params);
    remain = try ctx.valtypesRead(remain, "result", rs.types, null);

    return TypeUse{ .val = .{ .params = ps.types, .results = rs.types }, .params = params, .remain = remain };
}
pub fn typeuse(ctx: *Ctx, args: []const Expr) !TypeUse {
    if (args.len > 0) if (p.asFuncNamed(args[0], "type")) |func| {
        const i = try indexFindByF(ctx.I.funcTypes, func);
        const sig = ctx.I.funcTypes.items[i].val;

        const params = try ctx.gpa().alloc(?u.Txt, sig.params.len);

        var remain = try ctx.valtypesCheck(args[1..], "param", sig.params, params);
        remain = try ctx.valtypesCheck(remain, "result", sig.results, null);

        return TypeUse{ .val = sig, .params = params, .remain = remain };
    };
    return ctx.typedef(args);
}

inline fn pop(ctx: *Ctx, comptime field: []const u8, import: bool) *@TypeOf(@field(ctx.m, field)[0]) {
    var i = &@field(ctx.I, field);
    if (import) {
        i.nextImport += 1;
        return &@field(ctx.m, field)[i.nextImport - 1];
    } else {
        i.nextLocal += 1;
        return &@field(ctx.m, field)[i.nextLocal - 1];
    }
}
inline fn m_allocator(ctx: *Ctx) std.mem.Allocator {
    return ctx.arena.allocator();
}
inline fn gpa(ctx: *Ctx) std.mem.Allocator {
    return ctx.arena.child_allocator;
}
