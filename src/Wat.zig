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
                if (m.index) |i| try writer.print(" at index {}", .{ i });
            }
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
        else => std.log.warn("Unhandled section {s} index. Ignoring it", .{ func.name })
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
        else => std.log.warn("Unhandled section {} {}. Ignoring it", .{ section, func })
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

        const insts = try ctx.valtypes(typ.remain, "local", locals.types, typ.params[n_p..], false);

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
fn valtypes(self: *Ctx, args: []const Expr, comptime name: u.Txt, types: []IR.Func.Valtype, ids: ?[]?u.Txt, check: bool) ![]const Expr {
    var i: usize = 0;
    var n: usize = 0;
    while (i < args.len): (i += 1) {
        const param = asFuncNamed(args[i], name) orelse break;
        if (ids) |slice|
            slice[n] = param.id;
        for (param.args) |arg| {
            const v = try valtype(arg);
            if (check and types[n] != v) {
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

    var remain = try ctx.valtypes(args[@boolToInt(templated)..], "param", val_params, params, templated);
    remain = try ctx.valtypes(remain, "result", val_results, null, templated);

    return TypeUse{
        .val = .{ .params = val_params, .returns = val_results },
        .params = params, .remain = remain
    };
}
fn nTypeuse(args: []const Expr) usize {
    var i: usize = 0;
    if (args.len > 0 and asFuncNamed(args[0], "type") != null)
        i += 1;
    while (i < args.len): (i += 1) {
        if (asFuncNamed(args[i], "param") == null) break;
    }
    while (i < args.len): (i += 1) {
        if (asFuncNamed(args[i], "result") == null) break;
    }
    return i;
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

        inline fn get(it: @This(), i: usize) !IR.Func.Valtype {
            return if (i < it.first.len) it.first[i]
                else if (i < it.first.len+it.second.len)
                    it.second[i-it.first.len]
                else error.NotFound;
        }
    },
    stack: std.ArrayListUnmanaged(Stack) = .{},

    const Stack = union(enum) {
        val: IR.Func.Valtype,
        /// can be down casted to .i64
        i32: usize, // offset of .i32_const
        //TODO: f32

        pub fn format(
            self: Stack,
            comptime _: []const u8,
            _: std.fmt.FormatOptions,
            w: anytype,
        ) !void {
            return w.writeAll(switch (self) {
                .val => |v| @tagName(v),
                .i32 => "i32+"
            });
        }

        inline fn eql(self: Stack, other: Stack) bool {
            return std.meta.eql(self, other)
                or (self == .i32 and other.eqlV(.i32))
                or (other == .i32 and self.eqlV(.i32));
        }
        inline fn eqlV(self: Stack, v: IR.Func.Valtype) bool {
            return self.eql(.{ .val = v });
        }
        fn eqlOrCast(got: *Stack, expect: IR.Func.Valtype, self: *Codegen) bool {
            return switch (got.*) {
                .val => |v| v == expect,
                .i32 => |at| switch (expect) {
                    .i32 => true,
                    .i64 => {
                        self.bytes.items[at] = IR.opcode(.i64_const);
                        got.* = .{ .val = .i64 };
                        return true;
                    },
                    else => false
                },
            };
        }
        inline fn of(v: IR.Func.Valtype) Stack {
            return .{ .val = v };
        }

        fn snapshot(self: *const Codegen) ![]const Stack {
            return self.ctx.gpa.dupe(Stack, self.stack.items);
        }
        fn restore(self: *Codegen, snap: []const Stack) !void {
            try self.stack.resize(self.ctx.gpa, snap.len);
            std.mem.copy(Stack, self.stack.items, snap);
        }
        inline fn release(self: *const Codegen, snap: []const Stack) void {
            self.ctx.gpa.free(snap);
        }

        inline fn err(self: *Codegen, expect: ?Stack, got: ?Stack, index: ?usize) !void {
            self.ctx.err = .{ .typeMismatch = .{
                .expect = expect, .got = got, .index = index
            } };
            return error.TypeMismatch;
        }
        fn eqlOrCastSlice(self: *Codegen, offset: usize, typs: []const IR.Func.Valtype) !void {
            std.debug.assert(self.stack.items.len >= offset);
            const st = self.stack.items[offset..];
            if (st.len > typs.len) return err(self, null, st[typs.len], typs.len);
            if (st.len < typs.len) return err(self, of(typs[st.len]), null, st.len);

            for (st) |*got, i|
                if (!got.eqlOrCast(typs[i], self))
                    return err(self, of(typs[i]), got.*, i);
        }
        fn eqlSliceV(self: *Codegen, st: []const Stack, typs: []const IR.Func.Valtype) !void {
            if (st.len > typs.len) return err(self, null, st[typs.len], typs.len);
            if (st.len < typs.len) return err(self, of(typs[st.len]), null, st.len);

            for (st) |got, i|
                if (!got.eqlV(typs[i]))
                    return err(self, of(typs[i]), got, i);
        }
        fn eqlSlice(self: *Codegen, st: []const Stack, typs: []const Stack) !void {
            if (st.len > typs.len) return err(self, null, st[typs.len], typs.len);
            if (st.len < typs.len) return err(self, typs[st.len], null, st.len);

            for (st) |got, i|
                if (!typs[i].eql(got))
                    return err(self, typs[i], got, i);
        }
        inline fn fits(self: *Codegen, st: []const Stack, typs: []const IR.Func.Valtype) !void {
            if (st.len < typs.len) return err(self, of(typs[st.len]), null, st.len);
        }
        fn endsWith(self: *Codegen, st: []const Stack, typs: []const IR.Func.Valtype) !void {
            try fits(self, st, typs);
            return eqlSliceV(self, st[st.len-typs.len..], typs);
        }
        fn endsWithOrCast(self: *Codegen, typs: []const IR.Func.Valtype) !void {
            try fits(self, self.stack.items, typs);
            return eqlOrCastSlice(self, self.stack.items.len-typs.len, typs);
        }
    };

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
        Signed, TypeMismatch, NotFound, NotPow2, NotValtype,
    };
    fn inst(codegen: *Codegen, expr: Expr) Error!void {
        codegen.ctx.at = expr;
        const operation = expr.val.asFunc() orelse Expr.Val.Func{
            .name = expr.val.asKeyword() orelse return error.NotOp };

        if (nameToOp(operation.name)) |op| {
            const do = switch (op) {
                .i32_const => comptime iNconst(.i32, 32),
                .i64_const => comptime iNconst(.i64, 64),
                //fN_const
                .i32_store => comptime tNstore(.i32, 4),
                .i64_store => comptime tNstore(.i64, 8),
                .f32_store => comptime tNstore(.f32, 4),
                .f64_store => comptime tNstore(.f64, 8),
                .i32_eqz => comptime tNtest(.i32),
                .i32_eq, .i32_ne,
                .i32_lt_s, .i32_lt_u,
                .i32_gt_s, .i32_gt_u,
                .i32_le_s, .i32_le_u,
                .i32_ge_s, .i32_ge_u
                    => comptime tNcompare(.i32),
                .i64_eqz => comptime tNtest(.i64),
                .i64_eq, .i64_ne,
                .i64_lt_s, .i64_lt_u,
                .i64_gt_s, .i64_gt_u,
                .i64_le_s, .i64_le_u,
                .i64_ge_s, .i64_ge_u
                    => comptime tNcompare(.i64),
                .f32_eq, .f32_ne,
                .f32_lt, .f32_gt,
                .f32_le, .f32_ge
                    => comptime tNcompare(.f32),
                .f64_eq, .f64_ne,
                .f64_lt, .f64_gt,
                .f64_le, .f64_ge
                    => comptime tNcompare(.f64),
                .i32_clz, .i32_ctz, .i32_popcnt,
                .i32_extend8_s, .i32_extend16_s
                    => comptime tNunary(.i32),
                .i32_add, .i32_sub,
                .i32_mul,
                .i32_div_s, .i32_div_u,
                .i32_rem_s, .i32_rem_u,
                .i32_and, .i32_or, .i32_xor,
                .i32_shl, .i32_shr_s, .i32_shr_u,
                .i32_rotl, .i32_rotr
                    => comptime tNbinary(.i32),
                .i64_clz, .i64_ctz, .i64_popcnt,
                .i64_extend8_s, .i64_extend16_s, .i64_extend32_s
                    => comptime tNunary(.i64),
                .i64_add, .i64_sub,
                .i64_mul,
                .i64_div_s, .i64_div_u,
                .i64_rem_s, .i64_rem_u,
                .i64_and, .i64_or, .i64_xor,
                .i64_shl, .i64_shr_s, .i64_shr_u,
                .i64_rotl, .i64_rotr
                    => comptime tNbinary(.i64),
                .f32_abs, .f32_neg,
                .f32_ceil, .f32_floor,
                .f32_trunc, .f32_nearest,
                .f32_sqrt
                    => comptime tNunary(.f32),
                .f32_add, .f32_sub,
                .f32_mul, .f32_div,
                .f32_min, .f32_max,
                .f32_copysign
                    => comptime tNbinary(.f32),
                .f64_abs, .f64_neg,
                .f64_ceil, .f64_floor,
                .f64_trunc, .f64_nearest,
                .f64_sqrt
                    => comptime tNunary(.f64),
                .f64_add, .f64_sub,
                .f64_mul, .f64_div,
                .f64_min, .f64_max,
                .f64_copysign
                    => comptime tNbinary(.f64),
                .i32_wrap_i64 => comptime tNfrom(.i32, .i64),
                .i32_trunc_f32_s, .i32_trunc_f32_u
                    => comptime tNfrom(.i32, .f32),
                .i32_trunc_f64_s, .i32_trunc_f64_u
                    => comptime tNfrom(.i32, .f64),
                .i64_extend_i32_s, .i64_extend_i32_u
                    => comptime tNfrom(.i64, .i32),
                .i64_trunc_f32_s, .i64_trunc_f32_u
                    => comptime tNfrom(.i64, .f32),
                .i64_trunc_f64_s, .i64_trunc_f64_u
                    => comptime tNfrom(.i64, .f64),
                .f32_convert_i32_s, .f32_convert_i32_u
                    => comptime tNfrom(.f32, .i32),
                .f32_convert_i64_s, .f32_convert_i64_u
                    => comptime tNfrom(.f32, .i64),
                .f32_demote_f64 => comptime tNfrom(.f32, .f64),
                .f64_convert_i32_s, .f64_convert_i32_u
                    => comptime tNfrom(.f64, .i32),
                .f64_convert_i64_s, .f64_convert_i64_u
                    => comptime tNfrom(.f64, .i64),
                .f64_promote_f32 => comptime tNfrom(.f64, .f32),
                .i32_reinterpret_f32 => comptime tNfrom(.i32, .f32),
                .i64_reinterpret_f64 => comptime tNfrom(.i64, .f64),
                .f32_reinterpret_i32 => comptime tNfrom(.f32, .i32),
                .f64_reinterpret_i64 => comptime tNfrom(.f64, .i64),
                .local_get => comptime Op{ 
                    .narg = Op.Narg.id,
                    .gen = struct { fn gen(self: *Codegen, func: Expr.Val.Func) !void {
                        const i = if (func.id) |id|
                            for (self.locals.ids) |f, j| {
                                if (f != null and u.strEql(id, f.?))
                                    break @truncate(u32, j);
                            } else
                                return error.NotFound
                        else
                            try u32_(func.args[0]);

                        const typ = try self.locals.get(i);

                        try self.uleb(i);
                        try self.push(typ);
                    } }.gen
                },
                .nop => comptime Op{ },
                .call => comptime Op{
                    .narg = Op.Narg.id,
                    .gen = struct { fn gen(self: *Codegen, func: Expr.Val.Func) !void {
                        const i = if (func.id) |id|
                            for (self.ctx.m.funcs) |f, j| {
                                if (f.id != null and u.strEql(id, f.id.?))
                                    break @truncate(u32, j);
                            } else return error.NotFound
                        else
                            try u32_(func.args[0]);
                        
                        if (i > self.ctx.m.funcs.len) return error.NotFound;
                        const typ = self.ctx.m.funcs[i].type;

                        try self.pops(typ.params);
                        try self.uleb(i);
                        try self.pushs(typ.returns);
                    } }.gen
                },
                .drop => comptime Op{
                    .gen = struct { fn gen(self: *Codegen, _: Expr.Val.Func) !void {
                        _ = try self.pop();
                    } }.gen
                },
                .@"if" => comptime Op{
                    .narg = .{ .sf = struct { fn sf(f: Expr.Val.Func) []const Expr {
                        //TODO: label
                        const i = nTypeuse(f.args);
                        if (i >= f.args.len) return &[_]Expr{ };

                        //TODO: if else end syntax
                        const has_else = f.args.len > i+1;
                        const j = f.args.len-1-@boolToInt(has_else);

                        return f.args[i..j];
                    } }.sf },
                    .stack = .{ .pop = &[_]IR.Func.Valtype{ ibool } },
                    .gen = struct { fn gen(self: *Codegen, func: Expr.Val.Func) !void {
                        //TODO: label
                        const typ = try self.ctx.typeuse(func.args);
                        defer typ.deinit(self.ctx);
                        if (typ.remain.len == 0) return error.Empty;

                        //TODO: if else end syntax
                        const has_else = typ.remain.len > 1;
                        const j = typ.remain.len-1-@boolToInt(has_else);
                        const then = typ.remain[j];
                        const else_ = if (has_else) typ.remain[j+1] else null;
                        
                        const blocktype_offset = self.bytes.items.len;
                        try self.byte(std.wasm.block_empty);

                        const pre_stack = try Stack.snapshot(self);
                        defer Stack.release(self, pre_stack);

                        //FIXME: need to know min stack
                        try self.inst(then);

                        if (typ.val.params.len > 0 or typ.val.returns.len > 0) { // typed
                            std.debug.assert(pre_stack.len + typ.val.returns.len - typ.val.params.len == self.stack.items.len);
                            try Stack.endsWith(self, pre_stack, typ.val.params);
                            try Stack.endsWithOrCast(self, typ.val.returns);

                            // write blocktype_offset
                            const blocktype_p = &self.bytes.items[blocktype_offset];
                            if (typ.val.params.len == 0 and typ.val.returns.len <= 1) {
                                blocktype_p.* = if (typ.val.returns.len > 0)
                                    IR.Func.valtype(typ.val.returns[0]) else std.wasm.block_empty;
                            } else {
                                @panic("TODO: append to self.ctx.m.more_types");
                                // blocktype_p.* = typ_id;
                            }
                        } else {
                            @panic("TODO: type from self.stack.items - pre_stack");
                        }

                        if (else_) |do| {
                            try self.opcode(.@"else");

                            const then_stack = try Stack.snapshot(self);
                            defer Stack.release(self, then_stack);

                            try Stack.restore(self, pre_stack);
                            try self.inst(do);

                            try Stack.eqlSlice(self, self.stack.items, then_stack);
                        }

                        try self.opcode(.end);
                    } }.gen
                },
                //TODO: remove else branch
                else => {
                    std.log.warn("Unhandled {}. Ignoring it", .{ op });
                    return;
                }
            };

            // folded expr
            const folded = switch (do.narg) {
                .sf => |f| f(operation),
                else => take: {
                    const n = switch (do.narg) {
                        .n => |n| n,
                        .nf => |f| f(operation),
                        .sf => unreachable
                    };
                    if (n > operation.args.len) return error.Empty;
                    break :take operation.args[n..];
                }
            };
            for (folded) |fold|
                try codegen.inst(fold);

            try codegen.pops(do.stack.pop);
            try codegen.opcode(op);
            try do.gen(codegen, operation);
            try codegen.pushs(do.stack.push);

        } else {
            const CustomBinOp = enum {
                @"==", @"!=",
                @"<s", @"<u", @"<",
                @">s", @">u", @">",
                @"<=s", @"<=u", @"<=",
                @">=s", @">=u", @">=",
                @"+", @"-", @"*",
                @"/s", @"/u", @"/",
                @"%s", @"%u",
                @"&", @"|", @"^",
                @"<<", @">>s", @">>u",
                @"<<<", @">>>",

                const Gen = struct {
                    i32o: ?IR.Opcode, i64o: ?IR.Opcode, f32o: ?IR.Opcode, f64o: ?IR.Opcode,
                    bin: bool
                };
                inline fn tNtest(i32o: ?IR.Opcode, i64o: ?IR.Opcode, f32o: ?IR.Opcode, f64o: ?IR.Opcode) Gen {
                    return .{ .i32o = i32o, .i64o = i64o, .f32o = f32o, .f64o = f64o, .bin = false };
                }
                inline fn tNbin(i32o: ?IR.Opcode, i64o: ?IR.Opcode, f32o: ?IR.Opcode, f64o: ?IR.Opcode) Gen {
                    return .{ .i32o = i32o, .i64o = i64o, .f32o = f32o, .f64o = f64o, .bin = true };
                }
                inline fn iNtest(i32o: IR.Opcode, i64o: IR.Opcode) Gen { return @This().tNtest(i32o, i64o, null, null); }
                inline fn iNbin(i32o: IR.Opcode, i64o: IR.Opcode) Gen { return @This().tNbin(i32o, i64o, null, null); }
                inline fn fNtest(f32o: IR.Opcode, f64o: IR.Opcode) Gen { return @This().tNtest(null, null, f32o, f64o); }
            };
            if (std.meta.stringToEnum(CustomBinOp, operation.name)) |op| {
                for (operation.args) |fold|
                    try codegen.inst(fold);

                if (codegen.stack.items.len < 2)
                    return error.TypeMismatch;
                var tr = codegen.stack.pop();
                const tl = codegen.stack.pop();
                if (tl != .val or !tr.eqlOrCast(tl.val, codegen))
                    return Stack.err(codegen, tl, tr, null);

                // TODO: sign deduce
                const do = switch (op) {
                    .@"==" => CustomBinOp.tNtest(.i32_eq, .i64_eq, .f32_eq, .f64_eq),
                    .@"!=" => CustomBinOp.tNtest(.i32_ne, .i64_ne, .f32_ne, .f64_ne),
                    .@"<s" => CustomBinOp.iNtest(.i32_lt_s, .i64_lt_s),
                    .@"<u" => CustomBinOp.iNtest(.i32_lt_u, .i64_lt_u),
                    .@"<" => CustomBinOp.fNtest(.f32_lt, .f64_lt),
                    .@">s" => CustomBinOp.iNtest(.i32_gt_s, .i64_gt_s),
                    .@">u" => CustomBinOp.iNtest(.i32_gt_u, .i64_gt_u),
                    .@">" => CustomBinOp.fNtest(.f32_gt, .f64_gt),
                    .@"<=s" => CustomBinOp.iNtest(.i32_le_s, .i64_le_s),
                    .@"<=u" => CustomBinOp.iNtest(.i32_le_u, .i64_le_u),
                    .@"<=" => CustomBinOp.fNtest(.f32_le, .f64_le),
                    .@">=s" => CustomBinOp.iNtest(.i32_ge_s, .i64_ge_s),
                    .@">=u" => CustomBinOp.iNtest(.i32_ge_u, .i64_ge_u),
                    .@">=" => CustomBinOp.fNtest(.f32_ge, .f64_ge),
                    .@"+" => CustomBinOp.tNbin(.i32_add, .i64_add, .f32_add, .f64_add),
                    .@"-" => CustomBinOp.tNbin(.i32_sub, .i64_sub, .f32_sub, .f64_sub),
                    .@"*" => CustomBinOp.tNbin(.i32_mul, .i64_mul, .f32_mul, .f64_mul),
                    .@"/s" => CustomBinOp.iNbin(.i32_div_s, .i64_div_s),
                    .@"/u" => CustomBinOp.iNbin(.i32_div_u, .i64_div_u),
                    .@"/" => CustomBinOp.tNbin(null, null, .f32_div, .f64_div),
                    .@"%s" => CustomBinOp.iNbin(.i32_rem_s, .i64_rem_s),
                    .@"%u" => CustomBinOp.iNbin(.i32_rem_u, .i64_rem_u),
                    .@">>s" => CustomBinOp.iNbin(.i32_shr_s, .i64_shr_s),
                    .@">>u" => CustomBinOp.iNbin(.i32_shr_u, .i64_shr_u),
                    .@"&" => CustomBinOp.iNbin(.i32_and, .i64_and),
                    .@"|" => CustomBinOp.iNbin(.i32_or, .i64_or),
                    .@"^" => CustomBinOp.iNbin(.i32_xor, .i64_xor),
                    .@"<<" => CustomBinOp.iNbin(.i32_shl, .i64_shl),
                    .@"<<<" => CustomBinOp.iNbin(.i32_rotl, .i64_rotl),
                    .@">>>" => CustomBinOp.iNbin(.i32_rotr, .i64_rotr),
                };

                try codegen.opcode(switch (tl.val) {
                    .i32 => do.i32o,
                    .i64 => do.i64o,
                    .f32 => do.f32o,
                    .f64 => do.f64o,
                } orelse return Stack.err(codegen,
                    if (do.i32o != null) Stack{ .i32 = 0 } else Stack.of(.f32), tl, null));
                return codegen.push(if (do.bin) tl.val else .i32);
            }

            if (operation.name.len > 3) { // Short const
                const head = operation.name[0..operation.name.len-3];
                const tail = operation.name[operation.name.len-3..];
                if (std.meta.stringToEnum(IR.Func.Valtype, tail)) |typ| switch (typ) {
                    .i32 => if (i32_(kv(head)) catch null) |i| {
                        try codegen.opcode(.i32_const);
                        try codegen.ileb(i);

                        return codegen.push(typ);
                    },
                    .i64 => if (i64_(kv(head)) catch null) |i| {
                        try codegen.opcode(.i64_const);
                        try codegen.ileb(i);

                        return codegen.push(typ);
                    },
                    //TODO: remove else branch
                    else => @panic("WIP")
                };
            }
            //TODO: if (operation.name[-1] == 'f') Float litteral const
            // Int litteral const
            if (i64_(kv(operation.name)) catch null) |i| {
                if (i < std.math.maxInt(i32) and i > std.math.minInt(i32)) {
                    const at = codegen.bytes.items.len;
                    try codegen.opcode(.i32_const);
                    try codegen.stack.append(codegen.ctx.gpa, .{ .i32 = at });
                } else {
                    try codegen.opcode(.i64_const);
                    try codegen.push(.i64);
                }
                return codegen.ileb(i);
            }

            //TODO: more custom ops

            return error.NotOp;
        }
    }
    fn nameToOp(name: u.Txt) ?IR.Opcode {
        var buf: [32]u8 = undefined;
        const opname = if (std.mem.indexOfScalar(u8, name, '.')) |i| blk: {
            const opname = buf[0..name.len];
            std.mem.copy(u8, opname, name);
            opname[i] = '_';
            break :blk opname;
        } else name;
        return std.meta.stringToEnum(IR.Opcode, opname);
    }

    const Op = struct {
        narg: Narg = .{ .n = 0 },
        stack: struct {
            pop: []const IR.Func.Valtype = &[_]IR.Func.Valtype{},
            push: []const IR.Func.Valtype = &[_]IR.Func.Valtype{},
        } = .{},
        gen: fn(self: *Codegen, func: Expr.Val.Func) Error!void = genNop, 
    
        const Narg = union(enum) {
            n: usize,
            nf: fn (func: Expr.Val.Func) usize,
            sf: fn (func: Expr.Val.Func) []const Expr,

            const one = @This(){ .n = 1 };
            const id = @This(){
                .nf = struct { fn nf(f: Expr.Val.Func) usize {
                    return @boolToInt(f.id == null);
                } }.nf
            };
            const all = @This(){
                .nf = struct { fn nf(f: Expr.Val.Func) usize {
                    return f.args.len;
                } }.nf
            };
        };
        const genNop = struct { fn nop(_: *Codegen, _: Expr.Val.Func) !void { } }.nop;
    };
    inline fn iNconst(val: IR.Func.Valtype, comptime N: u16) Op {
        return .{
            .narg = Op.Narg.one,
            .stack = .{
                .push = &[_]IR.Func.Valtype{ val }
            },
            .gen = struct { fn Gen(comptime M: u16) type {
                return struct { fn gen(self: *Codegen, func: Expr.Val.Func) !void {
                    return self.ileb(try iN(func.args[0], M));
                } };
            } }.Gen(N-1).gen
        };
    }
    inline fn tNstore(val: IR.Func.Valtype, comptime align_: u32) Op {
        return .{
            .narg = .{ .nf = nMemarg },
            .stack = .{
                .pop = &[_]IR.Func.Valtype{ .i32, val },
            },
            .gen = struct { fn Gen(comptime align__: u32) type {
                return struct{ fn gen(self: *Codegen, func: Expr.Val.Func) !void {
                    const arg = try memarg(func.args, align__);
                    try self.uleb(arg.align_);
                    try self.uleb(arg.offset);
                } };
            } }.Gen(align_).gen,
        };
    }
    const ibool = .i32;
    inline fn tNtest(val: IR.Func.Valtype) Op {
        return .{ .stack = .{
            .pop = &[_]IR.Func.Valtype{ val },
            .push = &[_]IR.Func.Valtype{ ibool },
        } };
    }
    inline fn tNcompare(val: IR.Func.Valtype) Op {
        return .{ .stack = .{
            .pop = &[_]IR.Func.Valtype{ val, val },
            .push = &[_]IR.Func.Valtype{ ibool },
        } };
    }
    inline fn tNunary(val: IR.Func.Valtype) Op {
        return .{ .stack = .{
            .pop = &[_]IR.Func.Valtype{ val },
            .push = &[_]IR.Func.Valtype{ val },
        } };
    }
    inline fn tNbinary(val: IR.Func.Valtype) Op {
        return .{ .stack = .{
            .pop = &[_]IR.Func.Valtype{ val, val },
            .push = &[_]IR.Func.Valtype{ val },
        } };
    }
    inline fn tNfrom(to: IR.Func.Valtype, from: IR.Func.Valtype) Op {
        return .{ .stack = .{
            .pop = &[_]IR.Func.Valtype{ from },
            .push = &[_]IR.Func.Valtype{ to },
        } };
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
    fn nMemarg(func: Expr.Val.Func) usize {
        const exprs = func.args;
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
        try self.pops(results);
        try self.opcode(.end);
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
        try self.stack.append(self.ctx.gpa, .{ .val = typ });
    }
    fn pushs(self: *Codegen, typs: []const IR.Func.Valtype) !void {
        try self.stack.ensureUnusedCapacity(self.ctx.gpa, typs.len);
        for (typs) |typ|
            self.stack.appendAssumeCapacity(.{ .val = typ });
    }
    fn pop(self: *Codegen) !Stack {
        if (self.stack.items.len == 0)
            return error.TypeMismatch;
        return self.stack.pop();
    }
    fn pops(self: *Codegen, typs: []const IR.Func.Valtype) !void {
        try Stack.endsWithOrCast(self, typs);
        self.stack.items.len -= typs.len;
    }
};

pub const emit = @import("ToWat.zig").emit;
