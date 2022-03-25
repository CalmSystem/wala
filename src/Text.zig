const u = @import("util.zig");
const std = @import("std");
const Expr = @import("Expr.zig");
const IR = @import("IR.zig");
const p = @import("Text/parse.zig");
const Codegen = @import("Text/Codegen.zig");

const MODULE_KEY = Expr.Val{ .keyword = "module" };
const TopDefinition = enum { type, import, func, data, @"export", memory };

gpa: std.mem.Allocator,
m: IR.Module,
I: struct {
    funcs: IndexPos = .{},
    datas: usize = 0,
} = .{},
/// Parsing pointer
at: ?Expr = null,
err: ErrData = null,

const Ctx = @This();

const Indices = struct {
    funcs: UnmanagedImportableIndex = .{},
    memory: ?u.Txt = null,
    datas: UnmanagedIndex = .{},

    pub fn deinit(I: *Indices, allocator: std.mem.Allocator) void {
        I.funcs.it.deinit(allocator);
    }
};

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
const Result = union(enum) {
    ok: IR.Module,
    err: struct {
        kind: anyerror,
        data: ErrData = null,
        at: ?Expr = null,
    },
};
pub fn tryLoad(exprs: []const Expr, allocator: std.mem.Allocator, loader: anytype) Result {
    //TODO: load recussive with (use ?id name)
    _ = loader.load;

    var module: ?IR.Module = null;
    for (exprs) |expr| {
        const func = p.asFuncNamed(expr, MODULE_KEY.keyword) orelse continue;
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
    var ctx = Ctx{ .gpa = allocator, .m = IR.Module.init(allocator) };
    const module = ctx._loadModule(exprs) catch |err|
        return .{ .err = .{ .kind = err, .data = ctx.err, .at = ctx.at } };
    return .{ .ok = module };
}
fn _loadModule(ctx: *Ctx, exprs: []const Expr) !IR.Module {
    // name = if (exprs[0] dollared) ident else "env"

    var I: Indices = .{};
    for (exprs) |expr|
        try ctx.buildIndices(&I, expr, false);

    ctx.I.funcs.nextLocal = I.funcs.firstLocal;
    ctx.m.funcs = try ctx.m_allocator().alloc(IR.Func, I.funcs.it.items.len);
    for (I.funcs.it.items) |id, i| {
        ctx.m.funcs[i] = .{
            .id = try ctx.dupeN(id),
            .body = if (i < I.funcs.firstLocal) .{ .import = .{ .module = "", .name = "" } } else .{ .code = .{ .bytes = "" } },
            .type = .{},
        };
    }

    if (I.memory != null) {
        ctx.m.memory = .{ .id = try ctx.dupeN(I.memory), .size = .{ .min = 0, .max = null } };
    }

    ctx.m.datas = try ctx.m_allocator().alloc(IR.Data, I.datas.items.len);
    for (I.datas.items) |id, i| {
        ctx.m.datas[i] = .{ .id = try ctx.dupeN(id), .body = .{ .passive = "" } };
    }

    I.deinit(ctx.gpa);

    for (exprs) |expr|
        try ctx.loadDefinition(expr);

    return ctx.m;
}
inline fn buildIndices(ctx: *Ctx, I: *Indices, expr: Expr, comptime importParent: bool) !void {
    const func = expr.val.asFunc() orelse return;
    const section = std.meta.stringToEnum(TopDefinition, func.name) orelse return;
    if (func.args.len == 0) return error.Empty;

    ctx.at = expr;
    switch (section) {
        // .type => {
        //     if (importParent) return error.BadImport;
        //     //TODO: add to index
        // },
        .func => {
            const importAbbrev = try hasImportAbbrev(func.args, importParent);
            try I.funcs.append(ctx.gpa, func.id, importParent or importAbbrev);
        },
        //table
        .memory => {
            I.memory = func.id;
        },
        //global
        //elem if (importParent) return error.BadImport;
        .data => {
            if (importParent) return error.BadImport;
            try I.datas.append(ctx.gpa, func.id);
        },
        .import => {
            if (importParent) return error.BadImport;
            if (func.args.len != 3) return error.Empty;
            try ctx.buildIndices(I, func.args[2], true);
        },
        //TODO: remove else branch
        else => std.log.warn("Unhandled section {s} index. Ignoring it", .{func.name}),
    }
}

fn loadDefinition(ctx: *Ctx, expr: Expr) !void {
    ctx.at = expr;
    const func = expr.val.asFunc() orelse return error.NonFunctionTopValue;
    const section = std.meta.stringToEnum(TopDefinition, func.name) orelse return error.UnknownSection;

    switch (section) {
        .type => try ctx.loadType(func.args),
        .import => try ctx.loadImport(func.args),
        .func => try ctx.loadFunc(func.args, null),
        //TODO: table
        .memory => try ctx.loadMemory(func.args, null),
        //global
        //export
        //start
        //elem
        .data => try ctx.loadData(func.args),
        //TODO: remove else branch
        else => std.log.warn("Unhandled section {} {}. Ignoring it", .{ section, func }),
    }
}

fn loadType(_: *Ctx, _: []const Expr) !void {
    // Already done
}
fn loadImport(ctx: *Ctx, args: []const Expr) !void {
    if (args.len != 3) return error.Empty;
    const name = try ctx.importName(args[0..2]);

    ctx.at = args[2];
    const func = args[2].val.asFunc() orelse return error.BadImport;
    const kind = std.meta.stringToEnum(TopDefinition, func.name) orelse return error.BadImport;

    switch (kind) {
        .func => try ctx.loadFunc(func.args, name),
        //table
        .memory => try ctx.loadMemory(func.args, name),
        //global
        else => return error.BadImport,
    }
}
fn loadFunc(ctx: *Ctx, args: []const Expr, importParent: ?IR.ImportName) !void {
    const importAbbrev = try hasImportAbbrev(args, importParent != null);
    const f: *IR.Func = ctx.pop("funcs", importParent != null or importAbbrev);

    const io = try ctx.exportsImportNames(args, importAbbrev, f.id);
    f.exports = io.exports;
    if (io.import orelse importParent) |body|
        f.body = .{ .import = body };

    var typ = try ctx.typeuse(io.remain);
    defer typ.deinit(ctx);
    f.type = typ.val;

    if (importParent != null or importAbbrev) {
        if (typ.remain.len > 0)
            return error.ImportWithBody;
    } else {
        // locals
        const locals = try ctx.allocValtypes(typ.remain, "local");
        const n_p = typ.params.len;
        typ.params = try ctx.gpa.realloc(typ.params, n_p + locals.types.len);

        const insts = try ctx.valtypes(typ.remain, "local", locals.types, typ.params[n_p..], false);

        const code = try Codegen.load(ctx, insts, typ.val, locals.types, typ.params);
        f.body = .{ .code = code };
    }
}
//...
fn loadMemory(ctx: *Ctx, args: []const Expr, importParent: ?IR.ImportName) !void {
    const importAbbrev = try hasImportAbbrev(args, importParent != null);
    const id = if (ctx.m.memory) |m| m.id else null;
    const io = try ctx.exportsImportNames(args, importAbbrev, id);

    //TODO: data abbrev
    const memtyp = try limit(io.remain);

    ctx.m.memory = .{
        .id = id,
        .exports = io.exports,
        .import = io.import orelse importParent,
        .size = memtyp,
    };
}
//...
fn loadData(ctx: *Ctx, args: []Expr) !void {
    const d: *IR.Data = &ctx.m.datas[ctx.I.datas];
    ctx.I.datas += 1;

    var i: usize = 0;
    if (args.len > 0) {
        if (args[0].val.asFunc()) |first| { // active
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
}

const UnmanagedIndex = std.ArrayListUnmanaged(?u.Txt);
const UnmanagedImportableIndex = struct {
    it: UnmanagedIndex = .{},
    firstLocal: usize = 0,

    fn append(self: *UnmanagedImportableIndex, allocator: std.mem.Allocator, id: ?u.Txt, imported: bool) !void {
        if (imported) {
            try self.it.insert(allocator, self.firstLocal, id);
            self.firstLocal += 1;
        } else {
            try self.it.append(allocator, id);
        }
    }
    inline fn at(self: UnmanagedImportableIndex, idx: usize) ?u.Txt {
        return if (idx < self.it.items.len)
            self.it.items[idx]
        else
            null;
    }
};
const IndexPos = struct {
    nextImport: usize = 0,
    nextLocal: usize = 1 << 32,
};

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
    if (args.len > 2) return error.TooMay;
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
            0 => fallbackExport orelse return error.BadExport,
            else => return error.BadExport,
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
        ctx.gpa.free(self.params);
    }
};
const AllocVatypes = struct {
    types: []IR.Sigtype,
    remain: []const Expr,
};
fn allocValtypes(ctx: *Ctx, args: []const Expr, comptime name: u.Txt) !AllocVatypes {
    var i: usize = 0;
    var n: usize = 0;
    while (i < args.len) : (i += 1) {
        const param = p.asFuncNamed(args[i], name) orelse break;
        //TODO: abbrev ident
        n += param.args.len;
    }
    return AllocVatypes{ .types = try ctx.m_allocator().alloc(IR.Sigtype, n), .remain = args[i..] };
}
fn valtypes(self: *Ctx, args: []const Expr, comptime name: u.Txt, types: []IR.Sigtype, ids: ?[]?u.Txt, check: bool) ![]const Expr {
    var i: usize = 0;
    var n: usize = 0;
    while (i < args.len) : (i += 1) {
        const param = p.asFuncNamed(args[i], name) orelse break;
        if (ids) |slice|
            slice[n] = param.id;
        for (param.args) |arg| {
            self.at = arg;
            const v = p.enumKv(IR.Sigtype)(arg) orelse return error.NotValtype;
            if (check and !types[n].eql(v)) {
                self.err = .{ .typeMismatch = .{
                    .expect = .{ .val = types[n] },
                    .got = .{ .val = v },
                    .index = n,
                } };
                return error.TypeMismatch;
            }

            types[n] = v;
            n += 1;
            //TODO: abbrev ident
        }
    }
    std.debug.assert(n == types.len);
    return args[i..];
}
pub fn typeuse(ctx: *Ctx, args: []const Expr) !TypeUse {
    var ps: []IR.Sigtype = undefined;
    var rs: []IR.Sigtype = undefined;
    var templated = false;
    // TODO: templated type use
    // if (args.len > 0) {
    //     if (p.asFuncNamed(args[0], "type")) |func| {
    //         _ = func;
    //     }
    // }
    if (!templated) {
        // typeuse abbrev
        const params = try ctx.allocValtypes(args, "param");
        ps = params.types;

        const results = try ctx.allocValtypes(params.remain, "result");
        rs = results.types;
    }
    const params = try ctx.gpa.alloc(?u.Txt, ps.len);

    var remain = try ctx.valtypes(args[@boolToInt(templated)..], "param", ps, params, templated);
    remain = try ctx.valtypes(remain, "result", rs, null, templated);

    return TypeUse{ .val = .{ .params = ps, .results = rs }, .params = params, .remain = remain };
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
    return ctx.m.arena.allocator();
}

pub const emit = @import("ToWat.zig").emit;
