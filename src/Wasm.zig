const std = @import("std");
const u = @import("util.zig");
const IR = @import("IR.zig");

pub fn load(_: u.Bin, allocator: std.mem.Allocator) !IR.Module {
    //TODO: implem
    return IR.Module.init(allocator);
}

pub const Opt = struct {};
pub fn emit(m: IR.Module, writer: anytype, comptime opt: Opt) !void {
    const e = Emitter(@TypeOf(writer)){ .writer = writer, .m = &m, .opt = opt };

    try writer.writeAll(&std.wasm.magic);
    try writer.writeAll(&std.wasm.version);

    try e.section(.type);
    try e.section(.import);
    try e.section(.function);
    //TODO: try e.section(.table);
    try e.section(.memory);
    //TODO: try e.section(.global);
    try e.section(.@"export");
    try e.section(.code);
    try e.section(.data);

    //TODO: custom linking
}

fn Emitter(comptime Writer: type) type {
    return struct {
        writer: Writer,
        m: *const IR.Module,
        opt: Opt,

        const E = @This();

        pub fn section(e: E, comptime kind: std.wasm.Section) !void {
            // A bit too meta...
            const fn_name = @tagName(kind) ++ "Section";

            var size = std.io.countingWriter(std.io.null_writer);
            const se = Emitter(@TypeOf(size).Writer){ .m = e.m, .opt = e.opt, .writer = size.writer() };
            try @field(se, fn_name)();
            if (size.bytes_written == 0) return;

            try e.byte(std.wasm.section(kind));
            try e.uleb(size.bytes_written);
            try @field(e, fn_name)();
        }

        fn uleb(e: E, v: usize) !void {
            return std.leb.writeULEB128(e.writer, v);
        }
        fn string(e: E, str: u.Bin) !void {
            try e.uleb(str.len);
            try e.writer.writeAll(str);
        }
        fn byte(e: E, b: u8) !void {
            return e.writer.writeByte(b);
        }
        fn limits(e: E, l: std.wasm.Limits) !void {
            try e.byte(@boolToInt(l.max != null));
            try e.uleb(l.min);
            if (l.max) |max|
                try e.uleb(max);
        }

        fn typeSection(e: E) !void {
            //MAYBE: deduplicate
            //NOTE: reftype not handled...
            if (e.m.funcs.len == 0) return;

            try e.uleb(e.m.funcs.len);
            for (e.m.funcs) |func| {
                try e.byte(std.wasm.function_type);
                try e.uleb(func.type.params.len);
                for (func.type.params) |param|
                    try e.byte(std.wasm.valtype(param.lower()));
                try e.uleb(func.type.results.len);
                for (func.type.results) |ret|
                    try e.byte(std.wasm.valtype(ret.lower()));
            }
        }
        fn importSection(e: E) !void {
            var len: usize = 0;
            var iter = e.m.imports();
            while (iter.next() != null) len += 1;
            if (len == 0) return;

            try e.uleb(len);
            iter = e.m.imports();
            while (iter.next()) |cur| {
                try e.string(cur.key.module);
                try e.string(cur.key.name);
                try e.byte(@enumToInt(cur.kind));
                switch (cur.kind) {
                    .function => try e.uleb(cur.index),
                    .table => {
                        const t = &e.m.tables[cur.index];
                        try e.byte(std.wasm.reftype(t.type));
                        try e.limits(t.size);
                    },
                    .memory => try e.limits(e.m.memory.?.size),
                    .global => {
                        const g = &e.m.globals[cur.index];
                        try e.byte(std.wasm.valtype(g.type.lower()));
                        try e.byte(@boolToInt(g.mutable));
                    },
                }
            }
        }
        fn functionSection(e: E) !void {
            var len: usize = 0;
            for (e.m.funcs) |func| {
                switch (func.body) {
                    .code => len += 1,
                    else => {},
                }
            }
            if (len == 0) return;

            try e.uleb(len);
            for (e.m.funcs) |func, i| {
                switch (func.body) {
                    .code => try e.uleb(i),
                    else => {},
                }
            }
        }
        fn memorySection(e: E) !void {
            if (e.m.memory) |mem| if (mem.import == null) {
                try e.uleb(1);
                try e.limits(mem.size);
            };
        }
        fn exportSection(e: E) !void {
            var len: usize = 0;
            var iter = e.m.exports();
            while (iter.next() != null) len += 1;
            if (len == 0) return;

            iter = e.m.exports();
            try e.uleb(len);
            while (iter.next()) |cur| {
                try e.string(cur.key);
                try e.byte(@enumToInt(cur.kind));
                try e.uleb(cur.index);
            }
        }
        fn codeSection(e: E) !void {
            var len: usize = 0;
            for (e.m.funcs) |func| {
                switch (func.body) {
                    .code => len += 1,
                    else => {},
                }
            }
            if (len == 0) return;

            try e.uleb(len);
            for (e.m.funcs) |func| {
                switch (func.body) {
                    .code => |code| try e.string(code.bytes),
                    else => {},
                }
            }
        }
        fn dataSection(e: E) !void {
            if (e.m.datas.len == 0) return;

            try e.uleb(e.m.datas.len);
            for (e.m.datas) |data| {
                switch (data.body) {
                    .active => |act| {
                        if (act.mem == 0) {
                            try e.byte(0);
                        } else {
                            try e.byte(2);
                            try e.uleb(act.mem);
                        }
                        try e.writer.writeAll(act.offset.bytes);
                        try e.string(act.content);
                    },
                    .passive => |pas| {
                        try e.byte(1);
                        try e.string(pas);
                    },
                }
            }
        }
    };
}

pub const CodeReader = struct {
    it: std.io.FixedBufferStream(u.Bin),

    const Self = @This();

    pub fn init(code: u.Bin) Self {
        std.debug.assert(code.len > 1 and code[code.len - 1] == std.wasm.opcode(.end));
        return .{ .it = std.io.fixedBufferStream(code[0 .. code.len - 1]) };
    }
    inline fn reader(self: *Self) std.io.FixedBufferStream(u.Bin).Reader {
        return self.it.reader();
    }
    pub fn uleb32(self: *Self) !u32 {
        return std.leb.readULEB128(u32, self.reader());
    }
    fn byteToEnum(comptime Enum: type, tag: u8) ?Enum {
        inline for (std.meta.fields(Enum)) |field|
            if (tag == field.value)
                return @field(Enum, field.name);
        return null;
    }
    pub fn valtype(self: *Self) !IR.Valtype {
        const byte = try self.reader().readByte();
        return byteToEnum(IR.Valtype, byte) orelse return error.InvalidValue;
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
    pub fn next(self: *Self) !?Op {
        if (self.it.pos >= self.it.buffer.len) return null;

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
