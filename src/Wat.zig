const u = @import("util.zig");
const std = @import("std");
const Expr = @import("Expr.zig");
const IR = @import("IR.zig");

const MODULE_KEY = Expr.Val{ .keyword = "module" };
const TopDefinition = enum {
    type, import, func, data, @"export", memory
};

gpa: std.mem.Allocator,
m: IR.Module,
I: struct {
    funcs: IndexPos = .{},
    datas: usize = 0,
} = .{},
/// Parsing pointer
at: ?Expr = null,

const Ctx = @This();

const Indices = struct {
    funcs: UnmanagedImportableIndex = .{},
    memory: ?u.Txt = null,
    datas: UnmanagedIndex = .{},

    pub fn deinit(I: *Indices, allocator: std.mem.Allocator) void {
        I.funcs.it.deinit(allocator);
    }
};

const Result = union(enum) {
    ok: IR.Module,
    err: struct {
        kind: anyerror,
        at: ?Expr = null,
    },
};
pub fn tryLoad(exprs: []const Expr, allocator: std.mem.Allocator, loader: anytype) Result {
    //TODO: load recussive with (use ?id name)
    _ = loader.load;

    var module: ?IR.Module = null;
    for (exprs) |expr| {
        const func = asFuncNamed(expr, MODULE_KEY.keyword) orelse continue;
        const child = switch (tryLoadModule(func.args, allocator)) {
            .ok => |m| m,
            .err => |err| return .{ .err = err },
        };
        if (module) |parent| {
            module = IR.Module.link(&[_]IR.Module{parent, child}, allocator) catch |err|
                return .{ .err = .{ .kind = err } };
            parent.deinit();
        } else {
            module = child;
        }
    }

    return if (module) |m| .{ .ok = m }
        else // without top module
            tryLoadModule(exprs, allocator);
}
fn tryLoadModule(exprs: []const Expr, allocator: std.mem.Allocator) Result {
    var ctx = Ctx{ .gpa = allocator, .m = IR.Module.init(allocator) };
    const module = ctx._loadModule(exprs) catch |err|
        return .{ .err = .{ .kind = err, .at = ctx.at } };
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
            .body = if (i < I.funcs.firstLocal)
                .{ .import = .{ .module = "", .name = "" } }
                else .{ .code = .{ .bytes = "" } },
            .type = .{
                .params = &[_]std.wasm.Valtype{},
                .returns = &[_]std.wasm.Valtype{},
            },
        };
    }

    if (I.memory != null) {
        ctx.m.memory = .{
            .id = try ctx.dupeN(I.memory),
            .size = .{ .min = 0, .max = null }
        };
    }

    ctx.m.datas = try ctx.m_allocator().alloc(IR.Data, I.datas.items.len);
    for (I.datas.items) |id, i| {
        ctx.m.datas[i] = .{
            .id = try ctx.dupeN(id),
            .body = .{ .passive = "" }
        };
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
        else => std.log.warn("Unhandled section {s} index", .{ func.name })
    }
}

fn loadDefinition(ctx: *Ctx, expr: Expr) !void {
    ctx.at = expr;
    const func = expr.val.asFunc() orelse return error.NonFunctionTopValue;
    const section = std.meta.stringToEnum(TopDefinition, func.name) orelse return error.UnknownSection;

    switch(section) {
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
        else => std.log.warn("Unhandled section {} {}", .{ section, func })
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
        else => return error.BadImport
    }
}
fn loadFunc(ctx: *Ctx, args: []const Expr, importParent: ?IR.ImportName) !void {
    const importAbbrev = try hasImportAbbrev(args, importParent != null);
    const f: *IR.Func = ctx.pop("funcs", importParent != null or importAbbrev);

    const io = try ctx.exportsImportNames(args, importAbbrev);
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

        const insts = try valtypes(typ.remain, "local", locals.types, typ.params[n_p..], false);

        var codegen = try Codegen.initFunc(ctx, typ.val.params, locals.types, typ.params);
        defer codegen.deinit();

        for (insts) |i|
            try codegen.inst(i);
        try codegen.end(typ.val.returns);

        f.body = .{ .code = try codegen.dupe(ctx.m_allocator()) };
    }
}
//...
fn loadMemory(ctx: *Ctx, args: []const Expr, importParent: ?IR.ImportName) !void {
    const importAbbrev = try hasImportAbbrev(args, importParent != null);
    const io = try ctx.exportsImportNames(args, importAbbrev);

    //TODO: data abbrev
    const memtyp = try limit(io.remain);

    const id = if (ctx.m.memory) |m| m.id else null;
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
            const exprs = if (asFuncNamed(args[i], "offset")) |func|
                func.args else args[i..i+1];

            var codegen = Codegen.initExpr(ctx);
            defer codegen.deinit();
            for (exprs) |expr|
                try codegen.inst(expr);
            try codegen.end(&[_]IR.Func.Valtype{ .i32 });

            d.body = .{ .active = .{
                .mem = 0,
                .content = "",
                .offset = try codegen.dupe(ctx.m_allocator()),
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
        .active => |*act|
            act.content = datastring,
        .passive => |*pas|
            pas.* = datastring,
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
            self.it.items[idx] else null;
    }
};
const IndexPos = struct {
    nextImport: usize = 0,
    nextLocal: usize = 1<<32,
};

fn limit(args: []const Expr) !std.wasm.Limits {
    return switch(args.len) {
        0 => error.Empty,
        1 => .{ .min = try u32_(args[0]), .max = null },
        2 => .{ .min = try u32_(args[0]), .max = try u32_(args[1]) },
        else => error.TooMany
    };
}
fn hasImportAbbrev(args: []const Expr, importParent: bool) !bool {
    for (args) |arg| {
        if (asFuncNamed(arg, "import") != null) {
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
    return if(str) |v| try ctx.m_allocator().dupe(u8, v) else null;
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
fn exportsImportNames(ctx: *Ctx, args: []const Expr, withImport: bool) !ExportsImportName {
    var i: usize = 0;
    while (i < args.len and asFuncNamed(args[i], "export") != null): (i += 1) { }

    const exports = try ctx.m_allocator().alloc(IR.ExportName, i);
    i = 0;
    while (i < args.len): (i += 1) {
        const expor = asFuncNamed(args[i], "export") orelse break;
        ctx.at = args[i];
        if (expor.args.len != 1) return error.BadExport;
        //MAYBE: abbrev name from id
        exports[i] = try ctx.string(expor.args[0]);
    }

    var import: ?IR.ImportName = null;
    if (withImport) {
        ctx.at = args[i];
        const impor = asFuncNamed(args[i], "import") orelse return error.BadImport;
        import = try ctx.importName(impor.args);
        i += 1;
    }
    return ExportsImportName{ .exports = exports, .import = import, .remain = args[i..] };
}
const TypeUse = struct {
    val: IR.Func.Type,
    params: []?u.Txt,
    remain: []const Expr,

    pub inline fn deinit(self: TypeUse, ctx: *Ctx) void {
        ctx.gpa.free(self.params);
    }
};
const AllocVatypes = struct {
    types: []IR.Func.Valtype,
    remain: []const Expr,
};
fn allocValtypes(ctx: *Ctx, args: []const Expr, comptime name: u.Txt) !AllocVatypes {
    var i: usize = 0;
    var n: usize = 0;
    while (i < args.len): (i += 1) {
        const param = asFuncNamed(args[i], name) orelse break;
        //TODO: abbrev ident
        n += param.args.len;
    }
    return AllocVatypes{
        .types = try ctx.m_allocator().alloc(IR.Func.Valtype, n),
        .remain = args[i..]
    };
}
fn valtypes(args: []const Expr, comptime name: u.Txt, types: []IR.Func.Valtype, ids: ?[]?u.Txt, check: bool) ![]const Expr {
    var i: usize = 0;
    var n: usize = 0;
    while (i < args.len): (i += 1) {
        const param = asFuncNamed(args[i], name) orelse break;
        if (ids) |slice|
            slice[n] = param.id;
        for (param.args) |arg| {
            const v = try valtype(arg);
            if (check and types[n] != v)
                return error.TypeMismatch;

            types[n] = v;
            n += 1;
            //TODO: abbrev ident
        }
    }
    std.debug.assert(n == types.len);
    return args[i..];
}
fn typeuse(ctx: *Ctx, args: []const Expr) !TypeUse {
    var val_params: []IR.Func.Valtype = undefined;
    var val_results: []IR.Func.Valtype = undefined;
    var templated = false;
    // TODO: templated type use
    // if (args.len > 0) {
    //     if (asFuncNamed(args[0], "type")) |func| {
    //         _ = func;
    //     }
    // }
    if (!templated) {
        // typeuse abbrev
        const params = try ctx.allocValtypes(args, "param");
        val_params = params.types;

        const results = try ctx.allocValtypes(params.remain, "result");
        val_results = results.types;
    }
    const params = try ctx.gpa.alloc(?u.Txt, val_params.len);

    var remain = try valtypes(args[@boolToInt(templated)..], "param", val_params, params, templated);
    remain = try valtypes(remain, "result", val_results, null, templated);

    return TypeUse{
        .val = .{ .params = val_params, .returns = val_results },
        .params = params, .remain = remain
    };
}
fn valtype(arg: Expr) !std.wasm.Valtype {
    //TODO: reftype, vectype, interfacetype
    const key = arg.val.asKeyword() orelse return error.NotValtype;
    return std.meta.stringToEnum(std.wasm.Valtype, key) orelse return error.NotValtype;
}

fn asFuncNamed(arg: Expr, comptime name: u.Txt) ?Expr.Val.Func {
    const func = arg.val.asFunc() orelse return null;
    return if (u.strEql(name, func.name)) func else null;
}

inline fn pop(ctx: *Ctx, comptime field: []const u8, import: bool) *@TypeOf(@field(ctx.m, field)[0]) {
    var i = &@field(ctx.I, field);
    if (import) {
        i.nextImport += 1;
        return &@field(ctx.m, field)[i.nextImport-1];
    } else {
        i.nextLocal += 1;
        return &@field(ctx.m, field)[i.nextLocal-1];
    }
}
inline fn m_allocator(ctx: *Ctx) std.mem.Allocator {
    return ctx.m.arena.allocator();
}

fn UN(comptime N: u16) type {
    return std.meta.Int(.unsigned, N);
}
fn SN(comptime N: u16) type {
    return std.meta.Int(.signed, N);
}
fn IN(comptime N: u16) type {
    return SN(N+1);
}

inline fn digit(c: u8, hexnum: bool) !u8 {
    return switch(c) {
        '0'...'9' => c-'0',
        'A'...'F' => if (hexnum) c-'A'+10 else null,
        'a'...'f' => if (hexnum) c-'a'+10 else null,
        else => null
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

    while (i < str.len): (i += 1) {
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
    return iN(arg, N-1);
}
fn u32_(arg: Expr) !u32 {
    return uN(arg, 32);
}
fn i32_(arg: Expr) !i32 {
    return sN(arg, 32);
}
fn i64_(arg: Expr) !i64 {
    return sN(arg, 64);
}
inline fn kv(str: u.Txt) Expr {
    return .{ .val = .{ .keyword = str } };
}

const Codegen = struct {
    ctx: *Ctx,
    bytes: std.ArrayListUnmanaged(u8) = .{},
    relocs: std.ArrayListUnmanaged(IR.Linking.Reloc.Entry) = .{},
    locals: struct {
        first: []const IR.Func.Valtype,
        second: []const IR.Func.Valtype,
        ids: []const ?u.Txt,
    },
    stack: std.ArrayListUnmanaged(IR.Func.Valtype) = .{},

    fn initFunc(ctx: *Ctx, param_types: []const IR.Func.Valtype, local_types: []const IR.Func.Valtype, ids: []const ?u.Txt) !Codegen {
        var self = Codegen{ .ctx = ctx,
            .locals = .{ .first = param_types, .second = local_types, .ids = ids },
        };
        try self.reserve(1 + 2*local_types.len);
        try self.uleb(local_types.len);
        for (local_types) |local, i| {
            try self.uleb(param_types.len + i);
            try self.byte(IR.Func.valtype(local));
        }
        return self;
    }
    fn initExpr(ctx: *Ctx) Codegen {
        return Codegen{ .ctx = ctx,
            .locals = .{ .first = &[_]IR.Func.Valtype{}, .second = &[_]IR.Func.Valtype{}, .ids = &[_]?u.Txt{} },
        };
    }
    fn deinit(self: *Codegen) void {
        self.bytes.deinit(self.ctx.gpa);
        self.relocs.deinit(self.ctx.gpa);
        self.stack.deinit(self.ctx.gpa);
    }

    const Error = error{
        NotOp, Empty, NotDigit, Overflow, NotInt, OutOfMemory,
        Signed, TypeMismatch, NotFound, NotPow2,
    };
    fn inst(self: *Codegen, expr: Expr) Error!void {
        self.ctx.at = expr;
        const func = expr.val.asFunc() orelse Expr.Val.Func{
            .name = expr.val.asKeyword() orelse return error.NotOp };

        const op = nameToOp(func.name) orelse {
            if (func.name.len > 3) { // Short const
                const head = func.name[0..func.name.len-3];
                const tail = func.name[func.name.len-3..];
                if (std.meta.stringToEnum(IR.Func.Valtype, tail)) |typ| switch (typ) {
                    .i32 => if (i32_(kv(head)) catch null) |i| {
                        try self.opcode(IR.Opcode.i32_const);
                        try self.ileb(i);

                        return self.push(typ);
                    },
                    .i64 => if (i64_(kv(head)) catch null) |i| {
                        try self.opcode(IR.Opcode.i64_const);
                        try self.ileb(i);

                        return self.push(typ);
                    },
                    //TODO: remove else branch
                    else => @panic("WIP")
                };
            }

            //TODO: more custom ops

            return error.NotOp;
        };

        const take = switch (op) {
            .i32_const => 1,
            .i32_store => nMemarg(func.args),
            .call => @boolToInt(func.id == null),
            else => 0
        };
        if (take > func.args.len) return error.Empty;

        for (func.args[take..]) |fold|
            try self.inst(fold);

        switch (op) {
            .i32_const => {
                const i = try i32_(func.args[0]);

                try self.opcode(op);
                try self.ileb(i);

                try self.push(IR.Func.Valtype.i32);
            },
            .i64_const => {
                const i = try i64_(func.args[0]);

                try self.opcode(op);
                try self.ileb(i);

                try self.push(IR.Func.Valtype.i64);
            },
            .i32_store => {
                const arg = try memarg(func.args, 4);

                try self.pop(IR.Func.Valtype.i32);
                try self.pop(IR.Func.Valtype.i32);

                try self.opcode(op);
                try self.uleb(arg.align_);
                try self.uleb(arg.offset);
            },
            .i64_store => {
                const arg = try memarg(func.args, 8);

                try self.pop(IR.Func.Valtype.i32);
                try self.pop(IR.Func.Valtype.i64);

                try self.opcode(op);
                try self.uleb(arg.align_);
                try self.uleb(arg.offset);
            },
            .call => {
                var i: u32 = undefined;
                var typ: IR.Func.Type = undefined;
                if (func.id) |id| {
                    for (self.ctx.m.funcs) |f, j| {
                        if (f.id) |id2| {
                            if (u.strEql(id, id2)) {
                                typ = f.type;
                                i = @truncate(u32, j);
                                break;
                            }
                        }
                    } else
                        return error.NotFound;
                } else {
                    i = try u32_(func.args[0]);
                    if (i > self.ctx.m.funcs.len) return error.NotFound;
                    typ = self.ctx.m.funcs[i].type;
                }

                try self.pops(typ.params);

                try self.opcode(op);
                try self.uleb(i);

                try self.pushs(typ.returns);
            },
            .drop => {
                try self.pop(null);

                try self.opcode(op);
            },
            //TODO: remove else branch
            else => std.log.warn("Unhandled Op {}", .{ op })
        }
    }
    fn nameToOp(name: u.Txt) ?IR.Opcode {
        var buf: [32]u8 = undefined;
        if (name.len > buf.len) return null;
        const opname = buf[0..name.len];
        std.mem.copy(u8, opname, name);
        std.mem.replaceScalar(u8, opname, '.', '_');
        return std.meta.stringToEnum(IR.Opcode, opname);
    }
    fn kvarg(exprs: []const Expr, comptime key: u.Txt) ?u.Txt {
        if (exprs.len > 0) {
            if (exprs[0].val.asKeyword()) |k| {
                if (std.mem.startsWith(u8, k, key ++ "="))
                    return k[key.len+1..];
            }
        }
        return null;
    }
    fn nMemarg(exprs: []const Expr) usize {
        var i: usize = 0;
        if (kvarg(exprs, "offset") != null)
            i += 1;
        if (kvarg(exprs[i..], "align") != null)
            i += 1;
        return i;
    }
    const MemArg = struct {
        align_: u32,
        offset: u32 = 0,
    };
    fn memarg(exprs: []const Expr, n: u32) !MemArg {
        var i: usize = 0;
        const offset = if (kvarg(exprs, "offset")) |v|
            try u32_(kv(v)) else 0;
        const x = if (kvarg(exprs[i..], "align")) |v|
            try u32_(kv(v)) else n;
        if (x == 0 or (x & (x - 1)) != 0) return error.NotPow2;
        return MemArg{ .offset = offset, .align_ = @ctz(u32, x) };
    }

    fn end(self: *Codegen, results: []const IR.Func.Valtype) !void {
        if (self.bytes.items.len > 0 and self.bytes.items[self.bytes.items.len-1] == IR.opcode(IR.Opcode.end)) return;
        try self.pops(results);
        try self.opcode(IR.Opcode.end);
    }
    fn dupe(self: Codegen, allocator: std.mem.Allocator) !IR.Code {
        return IR.Code{
            .bytes = try allocator.dupe(u8, self.bytes.items),
            .relocs = try allocator.dupe(IR.Linking.Reloc.Entry, self.relocs.items),
        };
    }

    fn reserve(self: *Codegen, n: usize) !void {
        try self.bytes.ensureUnusedCapacity(self.ctx.gpa, n);
    }
    fn byte(self: *Codegen, b: u8) !void {
        try self.bytes.append(self.ctx.gpa, b);
    }
    inline fn writer(self: *Codegen) std.ArrayListUnmanaged(u8).Writer {
        return self.bytes.writer(self.ctx.gpa);
    }
    inline fn opcode(self: *Codegen, op: IR.Opcode) !void {
        try self.byte(IR.opcode(op));
    }
    fn uleb(self: *Codegen, v: u64) !void {
        try std.leb.writeULEB128(self.writer(), v);
    }
    fn ileb(self: *Codegen, v: i64) !void {
        try std.leb.writeILEB128(self.writer(), v);
    }

    fn push(self: *Codegen, typ: IR.Func.Valtype) !void {
        try self.stack.append(self.ctx.gpa, typ);
    }
    fn pushs(self: *Codegen, typs: []const IR.Func.Valtype) !void {
        try self.stack.appendSlice(self.ctx.gpa, typs);
    }
    fn pop(self: *Codegen, typ: ?IR.Func.Valtype) !void {
        if (self.stack.items.len == 0) return error.TypeMismatch;
        const o = self.stack.pop();
        if (typ) |t| if (t != o) return error.TypeMismatch;
    }
    fn pops(self: *Codegen, typs: []const IR.Func.Valtype) !void {
        if (self.stack.items.len < typs.len or !std.mem.eql(IR.Func.Valtype, typs,
            self.stack.items[self.stack.items.len-typs.len..])) return error.TypeMismatch;
        self.stack.items.len -= typs.len;
    }
};

pub const emit = @import("ToWat.zig").emit;
