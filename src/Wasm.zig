const std = @import("std");
const u = @import("util.zig");
const IR = @import("IR.zig");

const Emit = @import("Wasm/Emit.zig");
pub const Opt = Emit.Opt;
pub const emit = Emit.emit;

const Result = u.Result(IR.Module, struct {
    kind: anyerror,
    at: usize,
});
pub fn tryLoad(bin: u.Bin, allocator: std.mem.Allocator) Result {
    var reader = Reader.init(bin);
    const m = reader.wasm(allocator) catch |err| return .{ .err = .{ .kind = err, .at = reader.it.pos } };
    if (!reader.finished()) return .{ .err = .{ .kind = error.NotFinished, .at = reader.it.pos } };
    return .{ .ok = m };
}

fn byteToEnum(comptime Enum: type, tag: u8) ?Enum {
    inline for (std.meta.fields(Enum)) |field|
        if (tag == field.value)
            return @field(Enum, field.name);
    return null;
}
pub const Reader = struct {
    it: std.io.FixedBufferStream(u.Bin),

    const Self = @This();

    pub fn init(buffer: u.Bin) Self {
        return .{ .it = std.io.fixedBufferStream(buffer) };
    }
    pub fn wasm(self: *Self, allocator: std.mem.Allocator) !IR.Module {
        if (!u.strEql(&std.wasm.magic, try self.slice(std.wasm.magic.len)))
            return error.WrongMagic;
        if (!u.strEql(&std.wasm.version, try self.slice(std.wasm.version.len)))
            return error.UnsupportedVersion;

        var m = IR.Module.init(allocator);
        const m_allocator = m.arena.allocator();

        var types = std.ArrayListUnmanaged(IR.Func.Sig){};
        var funcs = std.ArrayListUnmanaged(IR.Func){};
        var tables = std.ArrayListUnmanaged(IR.Table){};
        var globals = std.ArrayListUnmanaged(IR.Global){};
        var customs = std.ArrayListUnmanaged(IR.Section.Custom){};
        defer {
            types.deinit(allocator);
            funcs.deinit(allocator);
            tables.deinit(allocator);
            globals.deinit(allocator);
            customs.deinit(allocator);
        }

        var prevKind = std.wasm.Section.custom; 
        while (!self.finished()) {
            const kind = try self.byteEnum(std.wasm.Section);
            const sectionEnd = try self.endPoint();
            switch (kind) {
                .custom => {
                    const name = try self.string(m_allocator);
                    std.log.info("Unknown custom section: {s}", .{ name });
                    //TODO: custom sections: name, producers, linking, debug
                    const content = try m_allocator.dupe(u8, try self.slice(sectionEnd-self.it.pos));

                    try customs.append(allocator, .{
                        .name = name, .content = content,
                        .after = prevKind,
                    });
                },
                .type => {
                    var n = try self.uleb32();
                    try types.ensureUnusedCapacity(allocator, n);
                    while (n > 0) : (n -= 1) {
                        if ((try self.reader().readByte()) != std.wasm.function_type)
                            return error.BadFuncType;

                        const ps = try m_allocator.alloc(IR.Sigtype, try self.uleb32());
                        var np: usize = 0;
                        while (np < ps.len) : (np += 1)
                            ps[np] = IR.Sigtype.upper(try self.valtype());
                        const rs = try m_allocator.alloc(IR.Sigtype, try self.uleb32());
                        np = 0;
                        while (np < rs.len) : (np += 1)
                            rs[np] = IR.Sigtype.upper(try self.valtype());
                        types.appendAssumeCapacity(.{ .params = ps, .results = rs });
                    }
                },
                .import => {
                    var n = try self.uleb32();
                    while (n > 0) : (n -= 1) {
                        const name = IR.ImportName{
                            .module = try self.string(m_allocator),
                            .name = try self.string(m_allocator),
                        };
                        switch (try self.byteEnum(std.wasm.ExternalKind)) {
                            .function => {
                                const typ = try self.uleb32();
                                if (typ >= types.items.len) return error.NotFound;
                                try funcs.append(allocator, .{
                                    .body = .{ .import = name },
                                    .type = types.items[typ],
                                });
                            },
                            else => unreachable, //FIXME:
                        }
                    }
                },
                .function => {
                    var n = try self.uleb32();
                    try funcs.ensureUnusedCapacity(allocator, n);
                    while (n > 0) : (n -= 1) {
                        const typ = try self.uleb32();
                        if (typ >= types.items.len) return error.NotFound;
                        funcs.appendAssumeCapacity(.{
                            .body = .{ .code = .{ .bytes = "" } },
                            .type = types.items[typ],
                        });
                    }
                },
                .table => {
                    var n = try self.uleb32();
                    try tables.ensureUnusedCapacity(allocator, n);
                    while (n > 0) : (n -= 1) {
                        const ref = try self.byteEnum(std.wasm.RefType);
                        const limit = try self.limits();
                        tables.appendAssumeCapacity(.{
                            .body = .intern,
                            .type = ref,
                            .size = limit,
                        });
                    }
                },
                .memory => {
                    const n = try self.uleb32();
                    std.debug.assert(n <= 1);
                    if (n > 0) {
                        std.debug.assert(m.memory == null);
                        m.memory = IR.Memory{ .id = null, .size = try self.limits() };
                    }
                },
                .global => {
                    var n = try self.uleb32();
                    try globals.ensureUnusedCapacity(allocator, n);
                    while (n > 0) : (n -= 1) {
                        const typ = try self.valtype();
                        const mut = try self.reader().readByte();
                        const code = try self.constExpr();
                        globals.appendAssumeCapacity(.{
                            .body = .{ .init = code },
                            .type = IR.Sigtype.upper(typ),
                            .mutable = mut == 1,
                        });
                    }
                },
                .@"export" => {
                    var n = try self.uleb32();
                    while (n > 0) : (n -= 1) {
                        const name = try self.string(m_allocator);
                        const desc = try self.byteEnum(std.wasm.ExternalKind);
                        const id = try self.uleb32();
                        const exs = switch (desc) {
                            .function => blk: {
                                if (id >= funcs.items.len) return error.NotFound;
                                break :blk &funcs.items[id].exports;
                            },
                            .table => blk: {
                                if (id >= tables.items.len) return error.NotFound;
                                break :blk &tables.items[id].exports;
                            },
                            .memory => blk: {
                                if (m.memory == null or id != 0) return error.NotFound;
                                break :blk &m.memory.?.exports;
                            },
                            .global => blk: {
                                if (id >= globals.items.len) return error.NotFound;
                                break :blk &globals.items[id].exports;
                            },
                        };
                        const new = try u.constSliceExpand(IR.ExportName, m_allocator, exs, 1);
                        new[0] = name;
                    }
                },
                .start => {
                    m.start = try self.uleb32();
                    if (m.start.? > funcs.items.len) return error.NotFound;
                },
                .element => {
                    var n = try self.uleb32();
                    var elems = try m_allocator.alloc(IR.Elem, n);
                    while (n > 0) : (n -= 1) {
                        const e = &elems[elems.len - n];
                        const flags = try self.reader().readByte();
                        const active = (flags & 0b1) == 0;
                        const indexed = (flags & 0b10) != 0;
                        const reftype = (flags & 0b100) != 0;
                        if (active) {
                            e.mode = .{ .active = .{
                                .table = if (indexed) try self.uleb32() else 0,
                                .offset = try self.constExpr(),
                            } };
                        } else {
                            e.mode = if (indexed) .declarative else .passive;
                        }

                        e.type = if (active and !indexed) .funcref else blk: {
                            const typ = try self.byteEnum(IR.Elem.Type);
                            if ((typ == .elemkind) == reftype)
                                return error.InvalidValue;
                            break :blk typ;
                        };
                        var nVals = try self.uleb32();
                        e.init = if (reftype) blk: {
                            const vals = try m_allocator.alloc(IR.InitExpr, nVals);
                            nVals = 0;
                            while (nVals < vals.len) : (nVals += 1)
                                vals[nVals] = try self.constExpr();
                            break :blk .{ .val = vals };
                        } else blk: {
                            const vals = try m_allocator.alloc(u32, nVals);
                            nVals = 0;
                            while (nVals < vals.len) : (nVals += 1)
                                vals[nVals] = try self.uleb32();
                            break :blk .{ .func = vals };
                        };
                    }
                    m.elements = elems;
                },
                .code => {
                    var n = try self.uleb32();
                    while (n > 0) : (n -= 1) {
                        const f: *IR.Func = for (funcs.items) |*a_f| {
                            if (a_f.body == .code and a_f.body.code.bytes.len == 0)
                                break a_f;
                        } else return error.NotFound;
                        const codeEnd = try self.endPoint();
                        const codeStart = self.it.pos;
                        const nLocals = try self.uleb32();
                        try self.it.seekBy(nLocals * 2);
                        var depth: usize = 1;
                        while (depth > 0) {
                            _ = try self.op(&depth);
                            //FIXME: bind types
                        }
                        try self.assertAt(codeEnd);
                        f.body = .{ .code = .{ .bytes = try m_allocator.dupe(u8, self.it.buffer[codeStart..codeEnd]) } };
                    }
                },
                .data => {
                    var n = try self.uleb32();
                    const new = try u.constSliceExpand(IR.Data, m_allocator, &m.datas, n);
                    while (n > 0) : (n -= 1) {
                        const seg = try self.reader().readByte();
                        new[new.len - n] = .{ .body = switch (seg) {
                            1 => .{ .passive = try self.byteVec(m_allocator) },
                            0, 2 => blk: {
                                if (seg == 2) {
                                    const mem = try self.uleb32();
                                    std.debug.assert(mem == 0);
                                }
                                const code = try self.constExpr();
                                break :blk .{ .active = .{ .mem = 0, .offset = code, .content = try self.byteVec(m_allocator) } };
                            },
                            else => return error.InvalidValue,
                        }, .id = null };
                    }
                },
                .data_count => {
                    // Ignore value
                    _ = try self.uleb32();
                },
                _ => {
                    std.log.warn("Unknown section kind: {}", .{ kind });
                    try self.it.seekTo(sectionEnd);
                },
            }
            try self.assertAt(sectionEnd);
            prevKind = kind;
        }

        m.funcs = try m_allocator.dupe(IR.Func, funcs.items);
        m.tables = try m_allocator.dupe(IR.Table, tables.items);
        m.globals = try m_allocator.dupe(IR.Global, globals.items);
        return m;
    }
    const Op = struct {
        op: IR.Code.Op,
        arg: union(enum) {
            none,
            blocktype: Blocktype,
            idx: u32,
            memarg: IR.Code.MemArg,
            int: i64,
            float: f64,
            table: struct {
                n: u32,
                /// (n+1) uleb. Last is default
                buf: u.Bin,
            },
            xy: [2]u32,
        },

        const Blocktype = union(enum) {
            empty,
            valtype: IR.Valtype,
            idx: u32,
        };
    };
    pub fn op(self: *Self, depth: ?*usize) !Op {
        const kind = try self.byteEnum(IR.Code.Op);
        const ret = Op{
            .op = kind,
            .arg = switch (kind) {
                .i32_const => .{ .int = try std.leb.readILEB128(i32, self.reader()) },
                .i64_const => .{ .int = try std.leb.readILEB128(i64, self.reader()) },
                .f32_const => .{ .float = @intToFloat(f32, try self.reader().readIntLittle(i32)) },
                .f64_const => .{ .float = @intToFloat(f64, try self.reader().readIntLittle(i64)) },
                .i32_load, .i64_load, .f32_load, .f64_load, .i32_load8_s, .i32_load8_u, .i32_load16_s, .i32_load16_u, .i64_load8_s, .i64_load8_u, .i64_load16_s, .i64_load16_u, .i64_load32_s, .i64_load32_u, .i32_store, .i64_store, .f32_store, .f64_store, .i32_store8, .i32_store16, .i64_store8, .i64_store16, .i64_store32 => .{ .memarg = .{ .align_ = try self.uleb32(), .offset = try self.uleb32() } },
                .local_get, .local_set, .local_tee, .global_get, .global_set, .call, .br, .br_if => .{ .idx = try self.uleb32() },
                .@"if", .block, .loop => blocktype: {
                    if (self.it.pos >= self.it.buffer.len) return error.EndOfStream;
                    const c = self.it.buffer[self.it.pos];
                    const blocktype = if (c == std.wasm.block_empty) blk: {
                        self.it.pos += 1;
                        break :blk .empty;
                    } else if (byteToEnum(IR.Valtype, c)) |v| blk: {
                        self.it.pos += 1;
                        break :blk Op.Blocktype{ .valtype = v };
                    } else Op.Blocktype{ .idx = try self.uleb32() };
                    break :blocktype .{ .blocktype = blocktype };
                },
                .@"unreachable", .nop, .drop, .@"else", .end, .@"return", .select, .i32_eqz, .i32_eq, .i32_ne, .i32_lt_s, .i32_lt_u, .i32_gt_s, .i32_gt_u, .i32_le_s, .i32_le_u, .i32_ge_s, .i32_ge_u, .i64_eqz, .i64_eq, .i64_ne, .i64_lt_s, .i64_lt_u, .i64_gt_s, .i64_gt_u, .i64_le_s, .i64_le_u, .i64_ge_s, .i64_ge_u, .f32_eq, .f32_ne, .f32_lt, .f32_gt, .f32_le, .f32_ge, .f64_eq, .f64_ne, .f64_lt, .f64_gt, .f64_le, .f64_ge, .i32_clz, .i32_ctz, .i32_popcnt, .i32_add, .i32_sub, .i32_mul, .i32_div_s, .i32_div_u, .i32_rem_s, .i32_rem_u, .i32_and, .i32_or, .i32_xor, .i32_shl, .i32_shr_s, .i32_shr_u, .i32_rotl, .i32_rotr, .i64_clz, .i64_ctz, .i64_popcnt, .i64_add, .i64_sub, .i64_mul, .i64_div_s, .i64_div_u, .i64_rem_s, .i64_rem_u, .i64_and, .i64_or, .i64_xor, .i64_shl, .i64_shr_s, .i64_shr_u, .i64_rotl, .i64_rotr, .f32_abs, .f32_neg, .f32_ceil, .f32_floor, .f32_trunc, .f32_nearest, .f32_sqrt, .f32_add, .f32_sub, .f32_mul, .f32_div, .f32_min, .f32_max, .f32_copysign, .f64_abs, .f64_neg, .f64_ceil, .f64_floor, .f64_trunc, .f64_nearest, .f64_sqrt, .f64_add, .f64_sub, .f64_mul, .f64_div, .f64_min, .f64_max, .f64_copysign, .i32_wrap_i64, .i32_trunc_f32_s, .i32_trunc_f32_u, .i32_trunc_f64_s, .i32_trunc_f64_u, .i64_extend_i32_s, .i64_extend_i32_u, .i64_trunc_f32_s, .i64_trunc_f32_u, .i64_trunc_f64_s, .i64_trunc_f64_u, .f32_convert_i32_s, .f32_convert_i32_u, .f32_convert_i64_s, .f32_convert_i64_u, .f32_demote_f64, .f64_convert_i32_s, .f64_convert_i32_u, .f64_convert_i64_s, .f64_convert_i64_u, .f64_promote_f32, .i32_reinterpret_f32, .i64_reinterpret_f64, .f32_reinterpret_i32, .f64_reinterpret_i64, .i32_extend8_s, .i32_extend16_s, .i64_extend8_s, .i64_extend16_s, .i64_extend32_s => .none,
                .memory_size, .memory_grow => blk: {
                    _ = try self.uleb32();
                    break :blk .none;
                },
                .br_table => blk: {
                    const n = try self.uleb32();
                    const start = self.it.pos;
                    var m: u32 = 0;
                    while (m <= n): (m += 1)
                        _ = try self.uleb32();
                    break :blk .{ .table = .{ .n = n, .buf = self.it.buffer[start..self.it.pos] } };
                },
                .call_indirect => blk: {
                    var xy: [2]u32 = undefined;
                    xy[0] = try self.uleb32();
                    xy[1] = try self.uleb32();
                    break :blk .{ .xy = xy };
                },
                //TODO: remove else branch
                else => blk: {
                    std.log.warn("Unhandled CodeRead {}", .{kind});
                    break :blk .none;
                },
            },
        };
        if (depth) |d| {
            if (ret.arg == .blocktype) d.* += 1 else if (ret.op == .end) d.* -= 1;
        }
        return ret;
    }

    inline fn reader(self: *Self) std.io.FixedBufferStream(u.Bin).Reader {
        return self.it.reader();
    }
    pub fn uleb32(self: *Self) !u32 {
        return std.leb.readULEB128(u32, self.reader());
    }
    fn byteEnum(self: *Self, comptime Enum: type) !Enum {
        const byte = try self.reader().readByte();
        return byteToEnum(Enum, byte) orelse return error.InvalidValue;
    }
    pub inline fn valtype(self: *Self) !IR.Valtype {
        return self.byteEnum(IR.Valtype);
    }
    fn slice(self: *Self, len: usize) !u.Bin {
        if (self.it.pos + len > self.it.buffer.len) return error.EndOfStream;
        const start = self.it.buffer[self.it.pos..];
        self.it.pos += len;
        return start[0..len];
    }
    fn byteVec(self: *Self, allocator: std.mem.Allocator) !u.Bin {
        return allocator.dupe(u8, try self.slice(try self.uleb32()));
    }
    fn string(self: *Self, allocator: std.mem.Allocator) !u.Txt {
        return u.toTxt(try self.byteVec(allocator));
    }
    fn limits(self: *Self) !std.wasm.Limits {
        return switch (try self.reader().readByte()) {
            0 => .{ .min = try self.uleb32(), .max = null },
            1 => .{ .min = try self.uleb32(), .max = try self.uleb32() },
            else => return error.InvalidValue,
        };
    }
    pub fn constExpr(self: *Self) !IR.InitExpr {
        const cons = try self.op(null);
        const end = try self.op(null);
        if (end.op != .end) return error.InvalidValue;
        return switch (cons.op) {
            .i32_const => .{ .i32_const = @truncate(i32, cons.arg.int) },
            .i64_const => .{ .i64_const = cons.arg.int },
            .f32_const => .{ .f32_const = @floatCast(f32, cons.arg.float) },
            .f64_const => .{ .f64_const = cons.arg.float },
            .global_get => .{ .global_get = cons.arg.idx }, //FIXME: check index
            else => return error.InvalidValue,
        };
    }

    fn endPoint(self: *Self) !usize {
        const len = try self.uleb32();
        return self.it.pos + len;
    }
    inline fn assertAt(self: Self, point: usize) !void {
        if (self.it.pos != point) return error.NotFinished;
    }
    pub fn finished(self: Self) bool {
        return self.it.pos >= self.it.buffer.len;
    }
};

test "empty module" {
    var empty: [8]u8 = undefined;
    std.mem.copy(u8, &empty, &std.wasm.magic);
    std.mem.copy(u8, empty[4..], &std.wasm.version);

    const result = tryLoad(&empty, std.testing.allocator);
    try std.testing.expect(result == .ok);
    const module = result.ok;
    defer module.deinit();

    var generated = try std.ArrayList(u8).initCapacity(std.testing.allocator, empty.len);
    defer generated.deinit();

    try emit(module, generated.writer(), .{});

    try std.testing.expectEqualSlices(u8, &empty, generated.items);
}
