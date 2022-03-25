const u = @import("../util.zig");
const std = @import("std");
const Expr = @import("../Expr.zig");
const IR = @import("../IR.zig");
const p = @import("parse.zig");
const Ctx = @import("../Text.zig");

ctx: *Ctx,
bytes: std.ArrayListUnmanaged(u8) = .{},
relocs: std.ArrayListUnmanaged(IR.Linking.Reloc.Entry) = .{},
locals: Locals,
stack: std.ArrayListUnmanaged(Stack) = .{},

const Codegen = @This();
const F = Expr.Val.Func;
const Hardtype = IR.Sigtype;

pub fn load(ctx: *Ctx, insts: []const Expr, typ: IR.Func.Sig, localTyps: ?[]const Hardtype, ids: ?[]const ?u.Txt) !IR.Code {
    var self = Codegen{
        .ctx = ctx,
        .locals = .{ .first = typ.params, .second = localTyps orelse &[_]Hardtype{}, .ids = ids orelse &[_]?u.Txt{} },
    };
    defer {
        self.bytes.deinit(self.ctx.gpa);
        self.relocs.deinit(self.ctx.gpa);
        self.stack.deinit(self.ctx.gpa);
    }
    if (localTyps) |local_types| {
        try self.reserve(1 + 2 * local_types.len);
        try self.uleb(local_types.len);
        for (local_types) |local, i| {
            try self.uleb(typ.params.len + i);
            try self.byte(std.wasm.valtype(local.lower()));
        }
    }

    for (insts) |i|
        try self.inst(i);
    try self.end(typ.results);

    return self.dupe(ctx.m.arena.allocator());
}

const Error = error{
    NotOp,
    Empty,
    NotDigit,
    Overflow,
    NotInt,
    OutOfMemory,
    Signed,
    TypeMismatch,
    NotFound,
    NotPow2,
    NotValtype,
};
fn inst(codegen: *Codegen, expr: Expr) Error!void {
    codegen.ctx.at = expr;
    const operation = codegen.extractFunc(expr.val) orelse return error.NotOp;

    if (nameToOp(operation.name)) |op| {
        const do = Op.get(op) orelse return;

        // folded expr
        const folded = switch (do.narg) {
            .sf => |f| f(operation),
            else => take: {
                const n = switch (do.narg) {
                    .n => |n| n,
                    .nf => |f| f(operation),
                    .sf => unreachable,
                };
                if (n > operation.args.len) return error.Empty;
                break :take operation.args[n..];
            },
        };
        for (folded) |fold|
            try codegen.inst(fold);

        try codegen.pops(do.stack.pop);
        try codegen.opcode(op);
        try do.gen(codegen, operation);
        try codegen.pushs(do.stack.push);
    } else if (std.meta.stringToEnum(CustomBinOp, operation.name)) |op| {
        for (operation.args) |fold|
            try codegen.inst(fold);

        if (codegen.stack.items.len < 2)
            return error.TypeMismatch;
        var tr = codegen.stack.pop();
        const tl = codegen.stack.pop();
        if (tl != .val or !tr.eqlOrCast(tl.val, codegen))
            return Stack.err(codegen, tl, tr, null);

        // TODO: sign deduce
        const do = CustomBinOp.get(op);

        try codegen.opcode(switch (tl.val) {
            .i32 => if (do.n == .i) do.n.i.i32o else null,
            .i64 => if (do.n == .i) do.n.i.i64o else null,
            .s8, .s16, .s32 => switch (do.n) {
                .i => |n| n.i32o,
                .su => |n| n.s32o,
            },
            .u8, .u16, .u32 => switch (do.n) {
                .i => |n| n.i32o,
                .su => |n| n.u32o,
            },
            .s64 => switch (do.n) {
                .i => |n| n.i64o,
                .su => |n| n.s64o,
            },
            .u64 => switch (do.n) {
                .i => |n| n.i64o,
                .su => |n| n.u64o,
            },
            .f32 => do.f32o,
            .f64 => do.f64o,
        } orelse return Stack.err(codegen, if (do.f32o != null) Stack.of(.f32) else Stack.of(.i32), tl, null));
        try codegen.push(if (do.bin) tl.val else .i32);
    } else {
        if (operation.name.len > 3) { // Short const
            const head = operation.name[0 .. operation.name.len - 3];
            const tail = operation.name[operation.name.len - 3 ..];
            if (std.meta.stringToEnum(Hardtype, tail)) |typ| switch (typ) {
                .i32, .s32, .u32 => if (p.i32s(head) catch null) |i| {
                    if (typ == .u32 and i < 0) return error.Signed;
                    try codegen.opcode(.i32_const);
                    try codegen.ileb(i);

                    return codegen.push(typ);
                },
                .i64, .s64, .u64 => if (p.i64s(head) catch null) |i| {
                    if (typ == .u64 and i < 0) return error.Signed;
                    try codegen.opcode(.i64_const);
                    try codegen.ileb(i);

                    return codegen.push(typ);
                },
                //TODO: remove else branch
                else => @panic("WIP"),
            };
        }

        if (operation.name.len > 0) { // num
            const tail = switch (operation.name[operation.name.len - 1]) {
                'i', 's', 'u', 'f' => operation.name[operation.name.len - 1],
                else => null,
            };
            const head = operation.name[0 .. operation.name.len - @boolToInt(tail != null)];
            //TODO: if (tail == 'f') Float litteral const

            // Int litteral const
            if (p.i64s(head) catch null) |i| {
                if (i < std.math.maxInt(i32) and i > std.math.minInt(i32)) {
                    const at = codegen.bytes.items.len;
                    try codegen.opcode(.i32_const);
                    const sign = switch (tail orelse 'i') {
                        's' => .s,
                        'u' => if (i < 0) return error.Signed else Stack.Sign.u,
                        else => if (i < 0) Stack.Sign.n else .p,
                    };
                    try codegen.stack.append(codegen.ctx.gpa, .{ .i32 = .{ .at = at, .sign = sign } });
                } else {
                    try codegen.opcode(.i64_const);
                    try codegen.push(.i64);
                }
                return codegen.ileb(i);
            }
        }
        //TODO: more custom ops

        return error.NotOp;
    }
}
inline fn extractFunc(codegen: *Codegen, val: Expr.Val) ?F {
    if (val.asFunc()) |f| return f;
    if (val.asKeyword()) |name| return F{ .name = name };

    switch (val) {
        .list => |exprs| if (exprs.len > 0) if (exprs[0].val.asId()) |id| {
            //TODO: detect collisions or specify resolution order
            const kind = if (codegen.locals.find(id) != null) @tagName(IR.Code.Op.local_get) else if (codegen.ctx.m.findFunc(id) != null) @tagName(IR.Code.Op.call) else if (codegen.ctx.m.findGlobal(id) != null) @tagName(IR.Code.Op.global_get) else null;
            if (kind) |name| return F{ .name = name, .id = id, .args = exprs[1..] };
        },
        else => {},
    }
    return null;
}
fn nameToOp(name: u.Txt) ?IR.Code.Op {
    var buf: [32]u8 = undefined;
    const opname = if (std.mem.indexOfScalar(u8, name, '.')) |i| blk: {
        const opname = buf[0..name.len];
        std.mem.copy(u8, opname, name);
        opname[i] = '_';
        break :blk opname;
    } else name;
    return std.meta.stringToEnum(IR.Code.Op, opname);
}

const Op = struct {
    narg: Narg = .{ .n = 0 },
    stack: struct {
        pop: []const Hardtype = &[_]Hardtype{},
        push: []const Hardtype = &[_]Hardtype{},
    } = .{},
    gen: fn (self: *Codegen, func: F) Error!void = genNop,

    const Narg = union(enum) {
        n: usize,
        nf: fn (func: F) usize,
        sf: fn (func: F) []const Expr,

        const one = @This(){ .n = 1 };
        const id = @This(){ .nf = struct {
            fn nf(f: F) usize {
                return @boolToInt(f.id == null);
            }
        }.nf };
        const all = @This(){ .nf = struct {
            fn nf(f: F) usize {
                return f.args.len;
            }
        }.nf };
    };
    const genNop = struct {
        fn nop(_: *Codegen, _: F) !void {}
    }.nop;

    inline fn get(op: IR.Code.Op) ?Op {
        return switch (op) {
            .i32_const => comptime iNconst(.i32, false),
            .i64_const => comptime iNconst(.i64, true),
            //fN_const
            .i32_store => comptime tNstore(.i32, 4),
            .i64_store => comptime tNstore(.i64, 8),
            .f32_store => comptime tNstore(.f32, 4),
            .f64_store => comptime tNstore(.f64, 8),
            .i32_eqz => comptime tNtest(.i32),
            .i32_eq, .i32_ne, .i32_lt_s, .i32_lt_u, .i32_gt_s, .i32_gt_u, .i32_le_s, .i32_le_u, .i32_ge_s, .i32_ge_u => comptime tNcompare(.i32),
            .i64_eqz => comptime tNtest(.i64),
            .i64_eq, .i64_ne, .i64_lt_s, .i64_lt_u, .i64_gt_s, .i64_gt_u, .i64_le_s, .i64_le_u, .i64_ge_s, .i64_ge_u => comptime tNcompare(.i64),
            .f32_eq, .f32_ne, .f32_lt, .f32_gt, .f32_le, .f32_ge => comptime tNcompare(.f32),
            .f64_eq, .f64_ne, .f64_lt, .f64_gt, .f64_le, .f64_ge => comptime tNcompare(.f64),
            .i32_clz, .i32_ctz, .i32_popcnt, .i32_extend8_s, .i32_extend16_s => comptime tNunary(.i32),
            .i32_add, .i32_sub, .i32_mul, .i32_div_s, .i32_div_u, .i32_rem_s, .i32_rem_u, .i32_and, .i32_or, .i32_xor, .i32_shl, .i32_shr_s, .i32_shr_u, .i32_rotl, .i32_rotr => comptime tNbinary(.i32),
            .i64_clz, .i64_ctz, .i64_popcnt, .i64_extend8_s, .i64_extend16_s, .i64_extend32_s => comptime tNunary(.i64),
            .i64_add, .i64_sub, .i64_mul, .i64_div_s, .i64_div_u, .i64_rem_s, .i64_rem_u, .i64_and, .i64_or, .i64_xor, .i64_shl, .i64_shr_s, .i64_shr_u, .i64_rotl, .i64_rotr => comptime tNbinary(.i64),
            .f32_abs, .f32_neg, .f32_ceil, .f32_floor, .f32_trunc, .f32_nearest, .f32_sqrt => comptime tNunary(.f32),
            .f32_add, .f32_sub, .f32_mul, .f32_div, .f32_min, .f32_max, .f32_copysign => comptime tNbinary(.f32),
            .f64_abs, .f64_neg, .f64_ceil, .f64_floor, .f64_trunc, .f64_nearest, .f64_sqrt => comptime tNunary(.f64),
            .f64_add, .f64_sub, .f64_mul, .f64_div, .f64_min, .f64_max, .f64_copysign => comptime tNbinary(.f64),
            .i32_wrap_i64 => comptime tNfrom(.i32, .i64),
            .i32_trunc_f32_s, .i32_trunc_f32_u => comptime tNfrom(.i32, .f32),
            .i32_trunc_f64_s, .i32_trunc_f64_u => comptime tNfrom(.i32, .f64),
            .i64_extend_i32_s, .i64_extend_i32_u => comptime tNfrom(.i64, .i32),
            .i64_trunc_f32_s, .i64_trunc_f32_u => comptime tNfrom(.i64, .f32),
            .i64_trunc_f64_s, .i64_trunc_f64_u => comptime tNfrom(.i64, .f64),
            .f32_convert_i32_s, .f32_convert_i32_u => comptime tNfrom(.f32, .i32),
            .f32_convert_i64_s, .f32_convert_i64_u => comptime tNfrom(.f32, .i64),
            .f32_demote_f64 => comptime tNfrom(.f32, .f64),
            .f64_convert_i32_s, .f64_convert_i32_u => comptime tNfrom(.f64, .i32),
            .f64_convert_i64_s, .f64_convert_i64_u => comptime tNfrom(.f64, .i64),
            .f64_promote_f32 => comptime tNfrom(.f64, .f32),
            .i32_reinterpret_f32 => comptime tNfrom(.i32, .f32),
            .i64_reinterpret_f64 => comptime tNfrom(.i64, .f64),
            .f32_reinterpret_i32 => comptime tNfrom(.f32, .i32),
            .f64_reinterpret_i64 => comptime tNfrom(.f64, .i64),
            .local_get => comptime Op{ .narg = Narg.id, .gen = struct {
                fn gen(self: *Codegen, func: F) !void {
                    const i = if (func.id) |id|
                        self.locals.find(id) orelse
                            return error.NotFound
                    else
                        try p.u32_(func.args[0]);

                    const typ = try self.locals.get(i);

                    try self.uleb(i);
                    try self.push(typ);
                }
            }.gen },
            .global_get => comptime Op{ .narg = Narg.id, .gen = struct {
                fn gen(self: *Codegen, func: F) !void {
                    const found = if (func.id) |id|
                        self.ctx.m.findGlobal(id) orelse
                            return error.NotFound
                    else blk: {
                        const i = try p.u32_(func.args[0]);
                        break :blk IR.Module.FoundId(IR.Global){ .ptr = &self.ctx.m.globals[i], .idx = i };
                    };

                    try self.uleb(found.idx);
                    try self.push(found.ptr.type);
                }
            }.gen },
            .nop => comptime Op{},
            .call => comptime Op{ .narg = Narg.id, .gen = struct {
                fn gen(self: *Codegen, func: F) !void {
                    const found = if (func.id) |id|
                        self.ctx.m.findFunc(id) orelse
                            return error.NotFound
                    else blk: {
                        const i = try p.u32_(func.args[0]);
                        if (i > self.ctx.m.funcs.len) return error.NotFound;
                        break :blk IR.Module.FoundId(IR.Func){ .ptr = &self.ctx.m.funcs[i], .idx = i };
                    };

                    try self.pops(found.ptr.type.params);
                    try self.uleb(found.idx);
                    try self.pushs(found.ptr.type.results);
                }
            }.gen },
            .drop => comptime Op{ .gen = struct {
                fn gen(self: *Codegen, _: F) !void {
                    _ = try self.pop();
                }
            }.gen },
            .@"if" => comptime Op{
                .narg = .{
                    .sf = struct {
                        fn sf(f: F) []const Expr {
                            //TODO: label
                            const i = p.nTypeuse(f.args);
                            if (i >= f.args.len) return &[_]Expr{};

                            //TODO: if else end syntax
                            const has_else = f.args.len > i + 1;
                            const j = f.args.len - 1 - @boolToInt(has_else);

                            return f.args[i..j];
                        }
                    }.sf,
                },
                .stack = .{ .pop = &[_]Hardtype{ibool} },
                .gen = struct {
                    fn gen(self: *Codegen, func: F) !void {
                        //TODO: label
                        const typ = try self.ctx.typeuse(func.args);
                        defer typ.deinit(self.ctx);
                        if (typ.remain.len == 0) return error.Empty;

                        //TODO: if else end syntax
                        const has_else = typ.remain.len > 1;
                        const j = typ.remain.len - 1 - @boolToInt(has_else);
                        const then = typ.remain[j];
                        const else_ = if (has_else) typ.remain[j + 1] else null;

                        const blocktype_offset = self.bytes.items.len;
                        try self.byte(std.wasm.block_empty);

                        const pre_stack = try Stack.snapshot(self);
                        defer Stack.release(self, pre_stack);

                        //FIXME: need to know min stack
                        try self.inst(then);

                        if (typ.val.params.len > 0 or typ.val.results.len > 0) { // typed
                            std.debug.assert(pre_stack.len + typ.val.results.len - typ.val.params.len == self.stack.items.len);
                            try Stack.endsWith(self, pre_stack, typ.val.params);
                            try Stack.endsWithOrCast(self, typ.val.results);

                            // write blocktype_offset
                            const blocktype_p = &self.bytes.items[blocktype_offset];
                            if (typ.val.params.len == 0 and typ.val.results.len <= 1) {
                                blocktype_p.* = if (typ.val.results.len > 0)
                                    std.wasm.valtype(typ.val.results[0].lower())
                                else
                                    std.wasm.block_empty;
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
                    }
                }.gen,
            },
            //TODO: remove else branch
            else => err: {
                std.log.warn("Unhandled {}. Ignoring it", .{op});
                break :err null;
            },
        };
    }

    inline fn iNconst(val: Hardtype, comptime is64: bool) Op {
        return .{ .narg = Op.Narg.one, .stack = .{ .push = &[_]Hardtype{val} }, .gen = struct {
            fn Gen(comptime Is64: bool) type {
                return struct {
                    fn gen(self: *Codegen, func: F) !void {
                        return self.ileb(try if (Is64) p.i64_(func.args[0]) else p.i32_(func.args[0]));
                    }
                };
            }
        }.Gen(is64).gen };
    }
    inline fn tNstore(val: Hardtype, comptime align_: u32) Op {
        return .{
            .narg = .{ .nf = nMemarg },
            .stack = .{
                .pop = &[_]Hardtype{ .i32, val },
            },
            .gen = struct {
                fn Gen(comptime align__: u32) type {
                    return struct {
                        fn gen(self: *Codegen, func: F) !void {
                            const arg = try memarg(func.args, align__);
                            try self.uleb(arg.align_);
                            try self.uleb(arg.offset);
                        }
                    };
                }
            }.Gen(align_).gen,
        };
    }
    inline fn tNtest(val: Hardtype) Op {
        return .{ .stack = .{
            .pop = &[_]Hardtype{val},
            .push = &[_]Hardtype{ibool},
        } };
    }
    inline fn tNcompare(val: Hardtype) Op {
        return .{ .stack = .{
            .pop = &[_]Hardtype{ val, val },
            .push = &[_]Hardtype{ibool},
        } };
    }
    inline fn tNunary(val: Hardtype) Op {
        return .{ .stack = .{
            .pop = &[_]Hardtype{val},
            .push = &[_]Hardtype{val},
        } };
    }
    inline fn tNbinary(val: Hardtype) Op {
        return .{ .stack = .{
            .pop = &[_]Hardtype{ val, val },
            .push = &[_]Hardtype{val},
        } };
    }
    inline fn tNfrom(to: Hardtype, from: Hardtype) Op {
        return .{ .stack = .{
            .pop = &[_]Hardtype{from},
            .push = &[_]Hardtype{to},
        } };
    }
};

const ibool = .i32;

const CustomBinOp = enum {
    @"==",
    @"!=",
    @"<s",
    @"<u",
    @"<",
    @">s",
    @">u",
    @">",
    @"<=s",
    @"<=u",
    @"<=",
    @">=s",
    @">=u",
    @">=",
    @"+",
    @"-",
    @"*",
    @"/s",
    @"/u",
    @"/",
    @"%s",
    @"%u",
    @"%",
    @"&",
    @"|",
    @"^",
    @"<<",
    @">>s",
    @">>u",
    @">>",
    @"<<<",
    @">>>",

    const Gen = struct { n: union(enum) { i: struct { i32o: ?IR.Code.Op, i64o: ?IR.Code.Op }, su: struct { s32o: ?IR.Code.Op, s64o: ?IR.Code.Op, u32o: ?IR.Code.Op, u64o: ?IR.Code.Op } }, f32o: ?IR.Code.Op, f64o: ?IR.Code.Op, bin: bool };

    inline fn get(op: @This()) Gen {
        // Good luck compiler
        @setEvalBranchQuota(100000);
        return switch (op) {
            .@"==" => comptime ifNbin("eq"),
            .@"!=" => comptime ifNbin("ne"),
            .@"<s" => comptime iNtest("lt_s"),
            .@"<u" => comptime iNtest("lt_u"),
            .@"<" => comptime sufNtest("lt"),
            .@">s" => comptime iNtest("gt_s"),
            .@">u" => comptime iNtest("gt_u"),
            .@">" => comptime sufNtest("gt"),
            .@"<=s" => comptime iNtest("le_s"),
            .@"<=u" => comptime iNtest("le_u"),
            .@"<=" => comptime sufNtest("le"),
            .@">=s" => comptime iNtest("ge_s"),
            .@">=u" => comptime iNtest("ge_u"),
            .@">=" => comptime sufNtest("ge"),
            .@"+" => comptime ifNbin("add"),
            .@"-" => comptime ifNbin("sub"),
            .@"*" => comptime ifNbin("mul"),
            .@"/s" => comptime iNbin("div_s"),
            .@"/u" => comptime iNbin("div_u"),
            .@"/" => comptime sufN("div", true),
            .@"%s" => comptime iNbin("rem_s"),
            .@"%u" => comptime iNbin("rem_u"),
            .@"%" => comptime suNbin("rem"),
            .@">>s" => comptime iNbin("shr_s"),
            .@">>u" => comptime iNbin("shr_u"),
            .@">>" => comptime suNbin("shr"),
            .@"&" => comptime iNbin("and"),
            .@"|" => comptime iNbin("or"),
            .@"^" => comptime iNbin("xor"),
            .@"<<" => comptime iNbin("shl"),
            .@"<<<" => comptime iNbin("rotl"),
            .@">>>" => comptime iNbin("rotr"),
        };
    }

    inline fn opFromStr(comptime t: IR.Valtype, comptime s: u.Txt) IR.Code.Op {
        return std.meta.stringToEnum(IR.Code.Op, @tagName(t) ++ "_" ++ s) orelse @compileError("Missing op");
    }
    inline fn iN(comptime s: u.Txt, bin: bool) Gen {
        return .{ .n = .{ .i = .{ .i32o = opFromStr(.i32, s), .i64o = opFromStr(.i64, s) } }, .f32o = null, .f64o = null, .bin = bin };
    }
    inline fn ifNbin(comptime s: u.Txt) Gen {
        return .{ .n = .{ .i = .{ .i32o = opFromStr(.i32, s), .i64o = opFromStr(.i64, s) } }, .f32o = opFromStr(.f32, s), .f64o = opFromStr(.f64, s), .bin = true };
    }
    inline fn sufN(comptime s: u.Txt, bin: bool) Gen {
        return .{ .n = .{ .su = .{
            .s32o = opFromStr(.i32, s ++ "_s"),
            .s64o = opFromStr(.i64, s ++ "_s"),
            .u32o = opFromStr(.i32, s ++ "_u"),
            .u64o = opFromStr(.i64, s ++ "_u"),
        } }, .f32o = opFromStr(.f32, s), .f64o = opFromStr(.f64, s), .bin = bin };
    }
    inline fn suNbin(comptime s: u.Txt) Gen {
        return .{ .n = .{ .su = .{
            .s32o = opFromStr(.i32, s ++ "_s"),
            .s64o = opFromStr(.i64, s ++ "_s"),
            .u32o = opFromStr(.i32, s ++ "_u"),
            .u64o = opFromStr(.i64, s ++ "_u"),
        } }, .f32o = null, .f64o = null, .bin = true };
    }
    inline fn iNbin(comptime s: u.Txt) Gen {
        return iN(s, true);
    }
    inline fn iNtest(comptime s: u.Txt) Gen {
        return iN(s, false);
    }
    inline fn sufNtest(comptime s: u.Txt) Gen {
        return sufN(s, false);
    }
};

fn kvarg(exprs: []const Expr, comptime key: u.Txt) ?u.Txt {
    if (exprs.len > 0) {
        if (exprs[0].val.asKeyword()) |k| {
            if (std.mem.startsWith(u8, k, key ++ "="))
                return k[key.len + 1 ..];
        }
    }
    return null;
}
fn nMemarg(func: F) usize {
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
        try p.u32s(v)
    else
        0;
    const x = if (kvarg(exprs[i..], "align")) |v|
        try p.u32s(v)
    else
        n;
    if (x == 0 or (x & (x - 1)) != 0) return error.NotPow2;
    return MemArg{ .offset = offset, .align_ = @ctz(u32, x) };
}

fn end(self: *Codegen, results: []const Hardtype) !void {
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
inline fn opcode(self: *Codegen, op: IR.Code.Op) !void {
    try self.byte(std.wasm.opcode(op));
}
fn uleb(self: *Codegen, v: u64) !void {
    try std.leb.writeULEB128(self.writer(), v);
}
fn ileb(self: *Codegen, v: i64) !void {
    try std.leb.writeILEB128(self.writer(), v);
}

fn push(self: *Codegen, typ: Hardtype) !void {
    try self.stack.append(self.ctx.gpa, .{ .val = typ });
}
fn pushs(self: *Codegen, typs: []const Hardtype) !void {
    try self.stack.ensureUnusedCapacity(self.ctx.gpa, typs.len);
    for (typs) |typ|
        self.stack.appendAssumeCapacity(.{ .val = typ });
}
fn pop(self: *Codegen) !Stack {
    if (self.stack.items.len == 0)
        return error.TypeMismatch;
    return self.stack.pop();
}
fn pops(self: *Codegen, typs: []const Hardtype) !void {
    try Stack.endsWithOrCast(self, typs);
    self.stack.items.len -= typs.len;
}

const Locals = struct {
    first: []const Hardtype,
    second: []const Hardtype,
    ids: []const ?u.Txt,

    inline fn get(it: @This(), i: usize) !Hardtype {
        return if (i < it.first.len) it.first[i] else if (i < it.first.len + it.second.len)
            it.second[i - it.first.len]
        else
            error.NotFound;
    }
    fn find(it: @This(), id: u.Txt) ?u32 {
        return for (it.ids) |f, j| {
            if (f != null and u.strEql(id, f.?))
                break @truncate(u32, j);
        } else null;
    }
};
pub const Stack = union(enum) {
    val: Hardtype,
    /// Can be implicitly converted to .i64_const op
    i32: struct {
        /// Offset of .i32_const op
        at: usize,
        sign: Sign,
    },
    //TODO: /// Can be implicitly converted to .f64_const op
    // f32: usize,

    const Sign = enum { p, n, s, u };
    pub fn format(
        self: Stack,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        w: anytype,
    ) !void {
        return w.writeAll(switch (self) {
            .val => |v| @tagName(v),
            .i32 => |v| switch (v.sign) {
                .p => "+N",
                .n => "-N",
                .s => "sN",
                .u => "uN",
            },
        });
    }

    inline fn eql(self: Stack, other: Stack) bool {
        return std.meta.eql(self, other) or (self == .i32 and other.eqlV(.i32)) or (other == .i32 and self.eqlV(.i32));
    }
    inline fn eqlV(self: Stack, v: Hardtype) bool {
        return self.eql(.{ .val = v });
    }
    fn eqlOrCast(got: *Stack, expect: Hardtype, self: *Codegen) bool {
        return switch (got.*) {
            .val => |v| v == expect,
            .i32 => |v| switch (expect) {
                //NOTE: i to s implicit cast is probably misleading
                .i32, .s32 => true,
                .u32 => v.sign == .p or v.sign == .u,
                .i64, .s64, .u64 => {
                    if (!switch (v.sign) {
                        .s => expect == .s64,
                        .u => expect == .u64,
                        .n => expect == .i64 or expect == .s64,
                        .p => true,
                    }) return false;
                    self.bytes.items[v.at] = std.wasm.opcode(.i64_const);
                    got.* = .{ .val = expect };
                    return true;
                },
                else => false,
            },
        };
    }
    inline fn of(v: Hardtype) Stack {
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
        self.ctx.err = .{ .typeMismatch = .{ .expect = expect, .got = got, .index = index } };
        return error.TypeMismatch;
    }
    fn eqlOrCastSlice(self: *Codegen, offset: usize, typs: []const Hardtype) !void {
        std.debug.assert(self.stack.items.len >= offset);
        const st = self.stack.items[offset..];
        if (st.len > typs.len) return err(self, null, st[typs.len], typs.len);
        if (st.len < typs.len) return err(self, of(typs[st.len]), null, st.len);

        for (st) |*got, i|
            if (!got.eqlOrCast(typs[i], self))
                return err(self, of(typs[i]), got.*, i);
    }
    fn eqlSliceV(self: *Codegen, st: []const Stack, typs: []const Hardtype) !void {
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
    inline fn fits(self: *Codegen, st: []const Stack, typs: []const Hardtype) !void {
        if (st.len < typs.len) return err(self, of(typs[st.len]), null, st.len);
    }
    fn endsWith(self: *Codegen, st: []const Stack, typs: []const Hardtype) !void {
        try fits(self, st, typs);
        return eqlSliceV(self, st[st.len - typs.len ..], typs);
    }
    fn endsWithOrCast(self: *Codegen, typs: []const Hardtype) !void {
        try fits(self, self.stack.items, typs);
        return eqlOrCastSlice(self, self.stack.items.len - typs.len, typs);
    }
};
