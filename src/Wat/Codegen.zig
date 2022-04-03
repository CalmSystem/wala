const u = @import("../util.zig");
const std = @import("std");
const Expr = @import("../Expr.zig");
const IR = @import("../IR.zig");
const p = @import("parse.zig");
const Ctx = @import("../Wat.zig");

ctx: *Ctx,
bytes: std.ArrayListUnmanaged(u8) = .{},
relocs: std.ArrayListUnmanaged(IR.Linking.Reloc.Entry) = .{},
types: std.ArrayListUnmanaged(IR.Func.Sig) = .{},
locals: Locals,
blocks: std.ArrayListUnmanaged(Block) = .{},
stack: std.ArrayListUnmanaged(Stack) = .{},

const Codegen = @This();
const Hardtype = IR.Sigtype;

pub fn load(ctx: *Ctx, insts: []const Expr, typ: IR.Func.Sig, localTyps: ?[]const Hardtype, ids: ?[]const ?u.Txt) !IR.Code {
    var self = Codegen{
        .ctx = ctx,
        .locals = .{ .first = typ.params, .second = localTyps orelse &[_]Hardtype{}, .ids = ids orelse &[_]?u.Txt{} },
    };
    defer {
        self.bytes.deinit(self.gpa());
        self.relocs.deinit(self.gpa());
        self.types.deinit(self.gpa());
        self.blocks.deinit(self.gpa());
        self.stack.deinit(self.gpa());
    }
    if (localTyps) |local_types| {
        try self.reserve(1 + 2 * local_types.len);
        try self.uleb(local_types.len);
        for (local_types) |local, i| {
            try self.uleb(typ.params.len + i);
            try self.byte(std.wasm.valtype(local.lower()));
        }
    }

    try self.block(.{ .label = null, .typ = typ.results }, insts);
    try self.end(typ.results);
    try self.isEmpty();

    return self.dupe(ctx.arena.allocator());
}

fn popInstsUntil(self: *Codegen, insts: []const Expr, comptime endKeywords: []const u.Txt) ![]const Expr {
    var remain = insts;
    while (remain.len > 0) {
        const e = remain[0].val.asList() orelse remain;
        if (e.len > 0) if (e[0].val.asKeyword()) |kv|
            inline for (endKeywords) |endK| if (u.strEql(endK, kv)) return remain;

        if (self.blocks.items[self.blocks.items.len - 1].typ == null)
            std.log.warn("Unreachable instructions {any}", .{remain});
        remain = try self.popInst(remain);
    }
    return remain;
}
inline fn popInsts(self: *Codegen, insts: []const Expr) !void {
    _ = try self.popInstsUntil(insts, &[_]u.Txt{});
}
fn blockUntil(self: *Codegen, it: Block, insts: []const Expr, comptime endKeywords: []const u.Txt) ![]const Expr {
    try self.blocks.append(self.gpa(), it);
    defer _ = self.blocks.pop();
    return self.popInstsUntil(insts, endKeywords);
}
inline fn block(self: *Codegen, it: Block, insts: []const Expr) !void {
    _ = try self.blockUntil(it, insts, &[_]u.Txt{});
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
    DuplicatedId,
    MultipleImport,
    NotString,
    TooMany,
    BadImport,
    BadExport,
    ImportWithBody,
    NoBlockEnd,
};

fn popInst(codegen: *Codegen, in: []const Expr) Error![]const Expr {
    std.debug.assert(in.len > 0);
    codegen.ctx.at = in[0];
    const folded = if (in[0].val.asList()) |l| l.len > 0 else false;
    const operation = (try codegen.extractFunc(if (folded) in[0].val.list else in, folded)) orelse return error.NotOp;
    const folded_rem = if (folded) in[1..] else null;

    if (nameToOp(operation.name)) |op| {
        const do = Op.get(op);

        const nargs = switch (do.narg) {
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
        if (folded) {
            try codegen.popInsts(nargs);
            codegen.ctx.at = in[0];
        }

        try codegen.pops(do.stack.pop);
        try codegen.opcode(op);
        const remain = (try do.gen(codegen, operation)) orelse
            (folded_rem orelse nargs);
        try codegen.pushs(do.stack.push);

        return remain;
    } else if (std.meta.stringToEnum(CustomBinOp, operation.name)) |op| {
        if (folded) {
            try codegen.popInsts(operation.args);
            codegen.ctx.at = in[0];
        }

        var tr = try codegen.pop();
        const tl = try codegen.pop();
        if (tl != .val or !tr.eqlOrCast(tl.val, codegen)) {
            codegen.ctx.err = .{ .typeMismatch = .{ .expect = tl, .got = tr } };
            return error.TypeMismatch;
        }

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
        } orelse {
            codegen.ctx.err = .{ .typeMismatch = .{ .expect = if (do.f32o != null) Stack.of(.f32) else Stack.of(.i32), .got = tl } };
            return error.TypeMismatch;
        });
        try codegen.push(if (do.bin) tl.val else .i32);

        return folded_rem orelse operation.args;
    } else if (!folded or operation.args.len == 0) { // keyword only
        if (operation.name.len > 3) { // Short const
            const head = operation.name[0 .. operation.name.len - 3];
            const tail = operation.name[operation.name.len - 3 ..];
            if (std.meta.stringToEnum(Hardtype, tail)) |typ| {
                switch (typ) {
                    .i32, .s32, .u32 => if (p.i32s(head) catch null) |i| {
                        if (typ == .u32 and i < 0) return error.Signed;
                        try codegen.opcode(.i32_const);
                        try codegen.ileb(i);

                        try codegen.push(typ);
                    },
                    .i64, .s64, .u64 => if (p.i64s(head) catch null) |i| {
                        if (typ == .u64 and i < 0) return error.Signed;
                        try codegen.opcode(.i64_const);
                        try codegen.ileb(i);

                        try codegen.push(typ);
                    },
                    //TODO: remove else branch
                    else => @panic("WIP"),
                }
                return operation.args;
            }
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
                    try codegen.stack.append(codegen.gpa(), .{ .i32 = .{ .at = at, .sign = sign } });
                } else {
                    try codegen.opcode(.i64_const);
                    try codegen.push(.i64);
                }
                try codegen.ileb(i);

                return operation.args;
            }
        }
        //TODO: more custom ops

    }
    return error.NotOp;
}
const F = struct {
    name: u.Txt,
    args: []const Expr,
    folded: bool,
};
inline fn extractFunc(codegen: *Codegen, in: []const Expr, folded: bool) !?F {
    return switch (in[0].val) {
        .keyword => |name| blk: {
            var f = F{ .name = name, .args = in[1..], .folded = folded };
            if (u.strEql("=", f.name)) {
                if (f.args.len == 0) return error.Empty;
                const id = f.args[0].val.asId() orelse return error.NotOp;
                f.name = switch (try codegen.idKind(id)) {
                    .local => @tagName(.local_set),
                    .global => @tagName(.global_set),
                    .func => return error.NotOp,
                };
            }
            break :blk f;
        },
        .id => |id| F{ .name = switch (try codegen.idKind(id)) {
            .local => @tagName(.local_get),
            .func => @tagName(.call),
            .global => @tagName(.global_get),
        }, .args = in, .folded = folded },
        else => null,
    };
}
fn idKind(codegen: *const Codegen, id: u.Txt) !enum { local, global, func } {
    const local = codegen.locals.find(id) != null;
    const call = Ctx.indexFindById(codegen.ctx.I.funcs, id) != null;
    const global = Ctx.indexFindById(codegen.ctx.I.globals, id) != null;
    if (@boolToInt(local) + @boolToInt(call) + @boolToInt(global) > 1)
        return error.DuplicatedId;
    if (local) return .local;
    if (call) return .func;
    if (global) return .global;
    return error.NotFound;
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
    gen: fn (self: *Codegen, func: F) Error!?[]const Expr = struct {
        fn unit(_: *Codegen, _: F) !?[]const Expr {
            return null;
        }
    }.unit,

    const Narg = union(enum) {
        n: usize,
        nf: fn (func: F) usize,
        sf: fn (func: F) []const Expr,

        const one = @This(){ .n = 1 };
    };

    inline fn get(op: IR.Code.Op) Op {
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
            .local_get => comptime Op{ .narg = Narg.one, .gen = struct {
                fn gen(self: *Codegen, func: F) !?[]const Expr {
                    const l = try self.locals.find_(func.args[0]);
                    try self.uleb(l.idx);
                    try self.push(l.typ);
                    return null;
                }
            }.gen },
            .local_set => comptime Op{ .narg = Narg.one, .gen = struct {
                fn gen(self: *Codegen, func: F) !?[]const Expr {
                    const l = try self.locals.find_(func.args[0]);
                    try self.uleb(l.idx);
                    try self.pops(&[_]Hardtype{l.typ});
                    return null;
                }
            }.gen },
            .local_tee => comptime Op{ .narg = Narg.one, .gen = struct {
                fn gen(self: *Codegen, func: F) !?[]const Expr {
                    const l = try self.locals.find_(func.args[0]);
                    try self.uleb(l.idx);
                    try self.pops(&[_]Hardtype{l.typ});
                    try self.push(l.typ);
                    return null;
                }
            }.gen },
            .global_get => comptime Op{
                .narg = Narg.one,
                .gen = struct {
                    fn gen(self: *Codegen, func: F) !?[]const Expr {
                        const gs = &self.ctx.I.globals;
                        const i = if (func.args[0].val.asId()) |id|
                            Ctx.indexFindById(gs, id) orelse
                                return error.NotFound
                        else
                            try p.u32_(func.args[0]);
                        if (i >= gs.items.len) return error.NotFound;

                        try self.uleb(i);
                        @panic("FIXME:");
                        // try self.push(self.ctx.loadGlobal().typ);
                        // return null;
                    }
                }.gen,
            },
            .nop => comptime Op{},
            .call => comptime Op{ .narg = Narg.one, .gen = struct {
                fn gen(self: *Codegen, func: F) Error!?[]const Expr {
                    const fs = &self.ctx.I.funcs;

                    const i = if (func.args[0].val.asId()) |id|
                        Ctx.indexFindById(fs, id) orelse
                            return error.NotFound
                    else
                        try p.u32_(func.args[0]);
                    if (i >= fs.items.len) return error.NotFound;

                    const ptr = try self.ctx.typeFunc(&fs.items[i].val, fs.items[i].id);
                    try self.pops(ptr.type.params);
                    try self.uleb(i);
                    try self.pushs(ptr.type.results);

                    return null;
                }
            }.gen },
            .drop => comptime Op{ .gen = struct {
                fn gen(self: *Codegen, _: F) !?[]const Expr {
                    _ = try self.pop();
                    return null;
                }
            }.gen },
            .@"if" => comptime Op{
                .narg = .{
                    .sf = struct {
                        fn sf(f: F) []const Expr {
                            if (!f.folded) return &[_]Expr{};

                            const has_label = f.args.len > 0 and f.args[0].val == .id;
                            var i: usize = @boolToInt(has_label);

                            i += p.nTypeuse(f.args[i..]);
                            if (i >= f.args.len) return &[_]Expr{};

                            //TODO: support folded else end syntax
                            const has_else = f.args.len > i + 1;
                            const j = f.args.len - 1 - @boolToInt(has_else);

                            return f.args[i..j];
                        }
                    }.sf,
                },
                .stack = .{ .pop = &[_]Hardtype{ibool} },
                .gen = struct {
                    fn gen(self: *Codegen, func: F) !?[]const Expr {
                        var remain = func.args;
                        if (remain.len == 0) return error.Empty;
                        const label = remain[0].val.asId();
                        if (label != null) remain = remain[1..];

                        const typ = try self.ctx.typeuse(remain);
                        defer typ.deinit(self.ctx);
                        remain = typ.remain;
                        if (remain.len == 0) return error.Empty;

                        //TODO: handle unknown types size (compact or not)
                        //TODO: handle unknown types values ([]?Hardtype)
                        try self.pops(typ.val.params);
                        if (typ.val.params.len == 0 and typ.val.results.len <= 1) {
                            try self.byte(if (typ.val.results.len > 0)
                                std.wasm.valtype(typ.val.results[0].lower())
                            else
                                std.wasm.block_empty);
                        } else {
                            const typ_id = @truncate(u32, self.types.items.len);
                            try self.types.append(self.gpa(), typ.val);
                            try self.relocs.append(self.gpa(), .{ .type = .typeIndexLeb, .offset = @truncate(u32, self.bytes.items.len), .index = typ_id });
                            try self.uleb5(typ_id);
                        }

                        const saved_stack = self.stack;
                        self.stack = .{};
                        try self.pushs(typ.val.params);

                        const a_block = Block{ .label = label, .typ = typ.val.results };
                        //TODO: support folded else end syntax
                        const has_else = if (func.folded) blk: {
                            const has_folded_else = remain.len > 1;
                            const j = typ.remain.len - 1 - @boolToInt(has_folded_else);
                            try self.block(a_block, &[_]Expr{typ.remain[j]});
                            break :blk has_folded_else;
                        } else blk: {
                            remain = try self.blockUntil(a_block, remain, &[_]u.Txt{ "else", "end" });
                            if (remain.len == 0) return error.NoBlockEnd;
                            const e = remain[0].val.asList() orelse remain;
                            remain = remain[1..]; // pop .else or .end
                            break :blk u.strEql("else", e[0].val.keyword);
                        };

                        try self.pops(typ.val.results);
                        try self.isEmpty();

                        if (has_else) {
                            try self.opcode(.@"else");

                            try self.pushs(typ.val.params);

                            if (func.folded) {
                                try self.block(a_block, &[_]Expr{typ.remain[typ.remain.len - 1]});
                            } else {
                                remain = try self.blockUntil(a_block, remain, &[_]u.Txt{"end"});
                                if (remain.len == 0) return error.NoBlockEnd;
                                remain = remain[1..]; // pop .end
                            }

                            try self.pops(typ.val.results);
                            try self.isEmpty();
                        } else if (typ.val.params.len != typ.val.results.len)
                            return error.TypeMismatch;
                        try self.opcode(.end);

                        self.stack.deinit(self.gpa());
                        self.stack = saved_stack;
                        try self.pushs(typ.val.results);

                        return if (func.folded) null else remain;
                    }
                }.gen,
            },
            .block, .loop => comptime Op{
                .narg = .{ .nf = struct {
                    fn all(func: F) usize {
                        return func.args.len;
                    }
                }.all },
                .gen = struct {
                    fn gen(self: *Codegen, func: F) !?[]const Expr {
                        //TODO: refactor duplicated code of if
                        var remain = func.args;
                        if (remain.len == 0) return error.Empty;
                        const label = remain[0].val.asId();
                        if (label != null) remain = remain[1..];

                        const typ = try self.ctx.typeuse(remain);
                        defer typ.deinit(self.ctx);
                        remain = typ.remain;
                        if (remain.len == 0) return error.Empty;

                        //TODO: handle unknown types size (compact or not)
                        //TODO: handle unknown types values ([]?Hardtype)
                        try self.pops(typ.val.params);
                        if (typ.val.params.len == 0 and typ.val.results.len <= 1) {
                            try self.byte(if (typ.val.results.len > 0)
                                std.wasm.valtype(typ.val.results[0].lower())
                            else
                                std.wasm.block_empty);
                        } else {
                            const typ_id = @truncate(u32, self.types.items.len);
                            try self.types.append(self.gpa(), typ.val);
                            try self.relocs.append(self.gpa(), .{ .type = .typeIndexLeb, .offset = @truncate(u32, self.bytes.items.len), .index = typ_id });
                            try self.uleb5(typ_id);
                        }

                        const saved_stack = self.stack;
                        self.stack = .{};
                        try self.pushs(typ.val.params);

                        const a_block = Block{ .label = label, .typ = typ.val.results };
                        if (func.folded) {
                            try self.block(a_block, remain);
                        } else {
                            remain = try self.blockUntil(a_block, remain, &[_]u.Txt{"end"});
                            if (remain.len == 0) return error.NoBlockEnd;
                            remain = remain[1..]; // pop .end
                        }

                        try self.pops(typ.val.results);
                        try self.isEmpty();

                        try self.opcode(.end);

                        self.stack.deinit(self.gpa());
                        self.stack = saved_stack;
                        try self.pushs(typ.val.results);

                        return if (func.folded) null else remain;
                    }
                }.gen,
            },
            .@"else", .end => comptime Op{
                .gen = struct {
                    fn gen(_: *Codegen, _: F) !?[]const Expr {
                        return error.NotOp;
                    }
                }.gen,
            },
            .br => comptime Op{
                .narg = Narg.one,
                .gen = struct {
                    fn gen(self: *Codegen, func: F) !?[]const Expr {
                        const b = try self.findBlock(func.args[0]);
                        if (b.val.typ) |s| try Stack.endsWithOrCast(self, s);
                        self.blockEndUnreachable();
                        try self.uleb(b.idx);
                        return null;
                    }
                }.gen,
            },
            .br_if => comptime Op{
                .narg = Narg.one,
                .stack = .{ .pop = &[_]Hardtype{ibool} },
                .gen = struct {
                    fn gen(self: *Codegen, func: F) !?[]const Expr {
                        const b = try self.findBlock(func.args[0]);
                        if (b.val.typ) |s| try Stack.endsWithOrCast(self, s);
                        try self.uleb(b.idx);
                        return null;
                    }
                }.gen,
            },
            .@"unreachable" => comptime Op{
                .gen = struct {
                    fn gen(self: *Codegen, _: F) !?[]const Expr {
                        self.blockEndUnreachable();
                        return null;
                    }
                }.gen,
            },
            //TODO: remove else branch
            else => {
                std.log.warn("Unhandled {}", .{op});
                unreachable;
            },
        };
    }

    inline fn iNconst(val: Hardtype, comptime is64: bool) Op {
        return .{ .narg = Narg.one, .stack = .{ .push = &[_]Hardtype{val} }, .gen = struct {
            fn Gen(comptime Is64: bool) type {
                return struct {
                    fn gen(self: *Codegen, func: F) !?[]const Expr {
                        try self.ileb(try if (Is64) p.i64_(func.args[0]) else p.i32_(func.args[0]));
                        return null;
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
                        fn gen(self: *Codegen, func: F) !?[]const Expr {
                            const arg = try memarg(func.args, align__);
                            try self.uleb(arg.align_);
                            try self.uleb(arg.offset);
                            return null;
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
const FoundBlock = struct {
    idx: usize,
    val: Block,
};
fn findBlock(self: *Codegen, expr: Expr) !FoundBlock {
    const bs = self.blocks.items;
    if (expr.val.asId()) |id| {
        // shadow older
        var i = bs.len;
        while (i > 0) : (i -= 1) {
            const b = bs[i - 1];
            if (b.label) |lb| if (u.strEql(id, lb))
                return FoundBlock{ .idx = bs.len - i, .val = b };
        }
        return error.NotFound;
    } else {
        const i = try p.u32_(expr);
        if (i >= bs.len) return error.NotFound;
        // 0 is current block
        return FoundBlock{ .idx = i, .val = bs[bs.len - 1 - i] };
    }
}
inline fn blockEndUnreachable(self: *Codegen) void {
    self.blocks.items[self.blocks.items.len - 1].typ = null;
}

fn end(self: *Codegen, results: []const Hardtype) !void {
    try self.pops(results);
    try self.opcode(.end);
}
fn dupe(self: Codegen, allocator: std.mem.Allocator) !IR.Code {
    return IR.Code{
        .bytes = try allocator.dupe(u8, self.bytes.items),
        .types = try allocator.dupe(IR.Func.Sig, self.types.items),
        .relocs = try allocator.dupe(IR.Linking.Reloc.Entry, self.relocs.items),
    };
}

inline fn gpa(self: *const Codegen) std.mem.Allocator {
    return self.ctx.arena.child_allocator;
}
fn reserve(self: *Codegen, n: usize) !void {
    try self.bytes.ensureUnusedCapacity(self.gpa(), n);
}
fn byte(self: *Codegen, b: u8) !void {
    try self.bytes.append(self.gpa(), b);
}
inline fn writer(self: *Codegen) std.ArrayListUnmanaged(u8).Writer {
    return self.bytes.writer(self.gpa());
}
inline fn opcode(self: *Codegen, op: IR.Code.Op) !void {
    try self.byte(std.wasm.opcode(op));
}
fn uleb(self: *Codegen, v: u64) !void {
    try std.leb.writeULEB128(self.writer(), v);
}
inline fn uleb5At(ptr: *[5]u8, v: u32) void {
    std.leb.writeUnsignedFixed(5, ptr, v);
}
/// uleb with always 5-byte
fn uleb5(self: *Codegen, v: u32) !void {
    var buf: [5]u8 = undefined;
    uleb5At(&buf, v);
    try self.writer().writeAll(&buf);
}
fn ileb(self: *Codegen, v: i64) !void {
    try std.leb.writeILEB128(self.writer(), v);
}

fn push(self: *Codegen, typ: Hardtype) !void {
    try self.stack.append(self.gpa(), .{ .val = typ });
}
fn pushs(self: *Codegen, typs: []const Hardtype) !void {
    try self.stack.ensureUnusedCapacity(self.gpa(), typs.len);
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
fn isEmpty(self: *Codegen) !void {
    const l = self.stack.items.len;
    if (l == 0) return;
    try Stack.err(self, null, self.stack.items[l - 1], l);
}

const Locals = struct {
    first: []const Hardtype,
    second: []const Hardtype,
    ids: []const ?u.Txt,

    fn find(it: @This(), id: u.Txt) ?u32 {
        return for (it.ids) |f, j| {
            if (f != null and u.strEql(id, f.?))
                break @truncate(u32, j);
        } else null;
    }
    const Found = struct {
        idx: usize,
        typ: Hardtype,
    };
    fn find_(it: @This(), expr: Expr) !Found {
        const i = if (expr.val.asId()) |id|
            it.find(id) orelse
                return error.NotFound
        else
            try p.u32_(expr);
        if (i >= it.first.len + it.second.len) return error.NotFound;

        return Found{ .idx = i, .typ = if (i < it.first.len) it.first[i] else it.second[i - it.first.len] };
    }
};
const Block = struct {
    label: ?u.Txt,
    /// null is unreachable
    typ: ?[]const Hardtype,
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
            .val => |v| switch (expect) {
                .i32, .i64 => v.lower() == expect.lower(),
                else => v == expect,
            },
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
