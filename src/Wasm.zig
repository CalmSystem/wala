const std = @import("std");
const u = @import("util.zig");
const IR = @import("IR.zig");

const Emit = @import("Wasm/Emit.zig");
pub const Opt = Emit.Opt;
pub const emit = Emit.emit;

pub fn load(bin: u.Bin, allocator: std.mem.Allocator) !IR.Module {
    var it = CodeReader.initBuffer(bin);
    if (!u.strEql(&std.wasm.magic, try it.readSlice(std.wasm.magic.len)))
        return error.WrongMagic;
    if (!u.strEql(&std.wasm.version, try it.readSlice(std.wasm.version.len)))
        return error.UnsupportedVersion;

    var m = IR.Module.init(allocator);

    var types = std.ArrayListUnmanaged(IR.Func.Sig){};
    var funcs = std.ArrayListUnmanaged(IR.Func){};
    defer {
        types.deinit(allocator);
        funcs.deinit(allocator);
    }
    // const customs = std.ArrayListUnmanaged(IR.Section.Custom){};

    while (!it.finished()) {
        const kind = byteToEnum(std.wasm.Section, try it.reader().readByte()) orelse return error.UnexpectedSection;
        var body = CodeReader.initBuffer(try it.readSlice(try it.uleb32()));
        switch (kind) {
            .type => {
                var n = try body.uleb32();
                try types.ensureUnusedCapacity(allocator, n);
                while (n > 0) : (n -= 1) {
                    if ((try body.reader().readByte()) != std.wasm.function_type)
                        return error.BadFuncType;

                    const ps = try m.arena.allocator().alloc(IR.Sigtype, try body.uleb32());
                    var np: usize = 0;
                    while (np < ps.len) : (np += 1)
                        ps[np] = IR.Sigtype.upper(try body.valtype());
                    const rs = try m.arena.allocator().alloc(IR.Sigtype, try body.uleb32());
                    np = 0;
                    while (np < rs.len) : (np += 1)
                        rs[np] = IR.Sigtype.upper(try body.valtype());
                    types.appendAssumeCapacity(.{ .params = ps, .results = rs });
                }
            },
            .import => {
                var n = try body.uleb32();
                while (n > 0) : (n -= 1) {
                    const name = IR.ImportName{
                        .module = try body.string(m.arena.allocator()),
                        .name = try body.string(m.arena.allocator()),
                    };
                    switch (byteToEnum(std.wasm.ExternalKind, try body.reader().readByte()) orelse return error.NotDesc) {
                        .function => {
                            const typ = try body.uleb32();
                            if (typ >= types.items.len) return error.NotFound;
                            try funcs.append(allocator, .{
                                .body = .{ .import = name },
                                .id = null,
                                .type = types.items[typ],
                            });
                        },
                        else => unreachable, //FIXME:
                    }
                }
            },
            .function => {
                var n = try body.uleb32();
                try funcs.ensureUnusedCapacity(allocator, n);
                while (n > 0) : (n -= 1) {
                    const typ = try body.uleb32();
                    if (typ >= types.items.len) return error.NotFound;
                    funcs.appendAssumeCapacity(.{
                        .body = .{ .code = .{ .bytes = "" } },
                        .id = null,
                        .type = types.items[typ],
                    });
                }
            },
            .memory => {
                const n = try body.uleb32();
                std.debug.assert(n <= 1);
                if (n > 0) {
                    std.debug.assert(m.memory == null);
                    m.memory = IR.Memory{ .id = null, .size = try body.limits() };
                }
            },
            .@"export" => {
                var n = try body.uleb32();
                while (n > 0) : (n -= 1) {
                    const name = try body.string(m.arena.allocator());
                    const desc = byteToEnum(std.wasm.ExternalKind, try body.reader().readByte()) orelse return error.NotDesc;
                    const id = try body.uleb32();
                    const exs = switch (desc) {
                        .function => blk: {
                            if (id >= funcs.items.len) return error.NotFound;
                            break :blk &funcs.items[id].exports;
                        },
                        .memory => blk: {
                            if (m.memory == null or id != 0) return error.NotFound;
                            break :blk &m.memory.?.exports;
                        },
                        else => unreachable, //FIXME:
                    };
                    const new = try u.constSliceExpand(IR.ExportName, m.arena.allocator(), exs, 1);
                    new[0] = name;
                }
            },
            .code => {
                var n = try body.uleb32();
                while (n > 0) : (n -= 1) {
                    const f: *IR.Func = for (funcs.items) |*a_f| {
                        if (a_f.body == .code and a_f.body.code.bytes.len == 0)
                            break a_f;
                    } else return error.NotFound;
                    const slice = try body.readSlice(try body.uleb32());
                    const code = try m.arena.allocator().dupe(u8, slice);
                    //FIXME: must iter to bundle types and check depth
                    f.body = .{ .code = .{ .bytes = code } };
                }
            },
            .data => {
                var n = try body.uleb32();
                const new = try u.constSliceExpand(IR.Data, m.arena.allocator(), &m.datas, n);
                while (n > 0) : (n -= 1) {
                    const seg = try body.reader().readByte();
                    new[new.len - n] = .{ .body = switch (seg) {
                        1 => .{ .passive = try body.byteVec(m.arena.allocator()) },
                        0, 2 => blk: {
                            if (seg == 2) {
                                const mem = try body.uleb32();
                                std.debug.assert(mem == 0);
                            }
                            var code = CodeReader.initBuffer(body.it.buffer[body.it.pos..]);
                            var depth: usize = 1;
                            while (depth > 0) {
                                const op = (try code.nextOp()) orelse return error.Empty;
                                if (op.arg == .blocktype) depth += 1;
                                if (op.op == .end) depth -= 1;
                            }
                            const offset = try m.arena.allocator().dupe(u8, try body.readSlice(code.it.pos));
                            break :blk .{ .active = .{ .mem = 0, .offset = .{ .bytes = offset }, .content = try body.byteVec(m.arena.allocator()) } };
                        },
                        else => return error.InvalidValue,
                    }, .id = null };
                }
            },
            else => {
                std.log.debug("{} {any}", .{ kind, body.it.buffer });
                return error.UnexpectedSection;
            },
        }
        std.debug.assert(body.finished());
    }

    m.funcs = try m.arena.allocator().dupe(IR.Func, funcs.items);
    return m;
}

fn byteToEnum(comptime Enum: type, tag: u8) ?Enum {
    inline for (std.meta.fields(Enum)) |field|
        if (tag == field.value)
            return @field(Enum, field.name);
    return null;
}
pub const CodeReader = struct {
    it: std.io.FixedBufferStream(u.Bin),

    const Self = @This();

    pub fn initBuffer(buffer: u.Bin) Self {
        return .{ .it = std.io.fixedBufferStream(buffer) };
    }
    pub fn initExpr(code: u.Bin) Self {
        std.debug.assert(code.len > 1 and code[code.len - 1] == std.wasm.opcode(.end));
        return initBuffer(code[0 .. code.len - 1]);
    }
    inline fn reader(self: *Self) std.io.FixedBufferStream(u.Bin).Reader {
        return self.it.reader();
    }
    pub fn uleb32(self: *Self) !u32 {
        return std.leb.readULEB128(u32, self.reader());
    }
    pub fn valtype(self: *Self) !IR.Valtype {
        const byte = try self.reader().readByte();
        return byteToEnum(IR.Valtype, byte) orelse return error.InvalidValue;
    }
    pub inline fn readSlice(self: *Self, len: usize) !u.Bin {
        if (self.it.pos + len > self.it.buffer.len) return error.EndOfStream;
        const start = self.it.buffer[self.it.pos..];
        self.it.pos += len;
        return start[0..len];
    }
    pub fn byteVec(self: *Self, allocator: std.mem.Allocator) !u.Bin {
        return allocator.dupe(u8, try self.readSlice(try self.uleb32()));
    }
    pub fn string(self: *Self, allocator: std.mem.Allocator) !u.Txt {
        return u.toTxt(try self.byteVec(allocator));
    }
    pub fn limits(self: *Self) !std.wasm.Limits {
        return switch (try self.reader().readByte()) {
            0 => .{ .min = try self.uleb32(), .max = null },
            1 => .{ .min = try self.uleb32(), .max = try self.uleb32() },
            else => return error.InvalidValue,
        };
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
        },

        const Blocktype = union(enum) {
            empty,
            valtype: IR.Valtype,
            idx: u32,
        };
    };
    pub inline fn finished(self: Self) bool {
        return self.it.pos >= self.it.buffer.len;
    }
    pub fn nextOp(self: *Self) !?Op {
        if (self.finished()) return null;

        const byte = try self.reader().readByte();
        const op = byteToEnum(IR.Code.Op, byte) orelse return error.InvalidValue;
        return Op{
            .op = op,
            .arg = switch (op) {
                .i32_const => .{ .int = try std.leb.readILEB128(i32, self.reader()) },
                .i64_const => .{ .int = try std.leb.readILEB128(i64, self.reader()) },
                .f32_const => .{ .float = @intToFloat(f32, try self.reader().readIntLittle(i32)) },
                .f64_const => .{ .float = @intToFloat(f64, try self.reader().readIntLittle(i64)) },
                .i32_load, .i64_load, .f32_load, .f64_load, .i32_load8_s, .i32_load8_u, .i32_load16_s, .i32_load16_u, .i64_load8_s, .i64_load8_u, .i64_load16_s, .i64_load16_u, .i64_load32_s, .i64_load32_u, .i32_store, .i64_store, .f32_store, .f64_store, .i32_store8, .i32_store16, .i64_store8, .i64_store16, .i64_store32 => .{ .memarg = .{ .align_ = try self.uleb32(), .offset = try self.uleb32() } },
                .local_get, .local_set, .local_tee, .call, .br, .br_if => .{ .idx = try self.uleb32() },
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
                .@"unreachable", .nop, .drop, .@"else", .end, .i32_eqz, .i32_eq, .i32_ne, .i32_lt_s, .i32_lt_u, .i32_gt_s, .i32_gt_u, .i32_le_s, .i32_le_u, .i32_ge_s, .i32_ge_u, .i64_eqz, .i64_eq, .i64_ne, .i64_lt_s, .i64_lt_u, .i64_gt_s, .i64_gt_u, .i64_le_s, .i64_le_u, .i64_ge_s, .i64_ge_u, .f32_eq, .f32_ne, .f32_lt, .f32_gt, .f32_le, .f32_ge, .f64_eq, .f64_ne, .f64_lt, .f64_gt, .f64_le, .f64_ge, .i32_clz, .i32_ctz, .i32_popcnt, .i32_add, .i32_sub, .i32_mul, .i32_div_s, .i32_div_u, .i32_rem_s, .i32_rem_u, .i32_and, .i32_or, .i32_xor, .i32_shl, .i32_shr_s, .i32_shr_u, .i32_rotl, .i32_rotr, .i64_clz, .i64_ctz, .i64_popcnt, .i64_add, .i64_sub, .i64_mul, .i64_div_s, .i64_div_u, .i64_rem_s, .i64_rem_u, .i64_and, .i64_or, .i64_xor, .i64_shl, .i64_shr_s, .i64_shr_u, .i64_rotl, .i64_rotr, .f32_abs, .f32_neg, .f32_ceil, .f32_floor, .f32_trunc, .f32_nearest, .f32_sqrt, .f32_add, .f32_sub, .f32_mul, .f32_div, .f32_min, .f32_max, .f32_copysign, .f64_abs, .f64_neg, .f64_ceil, .f64_floor, .f64_trunc, .f64_nearest, .f64_sqrt, .f64_add, .f64_sub, .f64_mul, .f64_div, .f64_min, .f64_max, .f64_copysign, .i32_wrap_i64, .i32_trunc_f32_s, .i32_trunc_f32_u, .i32_trunc_f64_s, .i32_trunc_f64_u, .i64_extend_i32_s, .i64_extend_i32_u, .i64_trunc_f32_s, .i64_trunc_f32_u, .i64_trunc_f64_s, .i64_trunc_f64_u, .f32_convert_i32_s, .f32_convert_i32_u, .f32_convert_i64_s, .f32_convert_i64_u, .f32_demote_f64, .f64_convert_i32_s, .f64_convert_i32_u, .f64_convert_i64_s, .f64_convert_i64_u, .f64_promote_f32, .i32_reinterpret_f32, .i64_reinterpret_f64, .f32_reinterpret_i32, .f64_reinterpret_i64, .i32_extend8_s, .i32_extend16_s, .i64_extend8_s, .i64_extend16_s, .i64_extend32_s => .none,
                //TODO: remove else branch
                else => blk: {
                    std.log.warn("Unhandled CodeRead {}", .{op});
                    break :blk .none;
                },
            },
        };
    }
};

test "empty module" {
    var empty: [8]u8 = undefined;
    std.mem.copy(u8, &empty, &std.wasm.magic);
    std.mem.copy(u8, empty[4..], &std.wasm.version);

    const module = try load(&empty, std.testing.allocator);
    defer module.deinit();

    var generated = try std.ArrayList(u8).initCapacity(std.testing.allocator, empty.len);
    defer generated.deinit();

    try emit(module, generated.writer(), .{});

    try std.testing.expectEqualSlices(u8, &empty, generated.items);
}
