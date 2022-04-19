const std = @import("std");
const IR = @import("../IR.zig");

pub const Opt = struct {};
pub fn emit(m: IR.Module, writer: anytype, comptime opt: Opt) !void {
    const e = Emitter(@TypeOf(writer)){ .writer = writer, .m = &m, .opt = opt };

    try writer.writeAll(&std.wasm.magic);
    try writer.writeAll(&std.wasm.version);

    try e.section(.type);
    try e.section(.import);
    try e.section(.function);
    try e.section(.table);
    try e.section(.memory);
    try e.section(.global);
    try e.section(.@"export");
    try e.section(.start);
    try e.section(.element);
    //MAYBE: data_count
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
        fn string(e: E, str: []const u8) !void {
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
        fn constExpr(e: E, expr: IR.InitExpr) !void {
            try e.byte(std.wasm.opcode(IR.initExpr(expr)));
            switch (expr) {
                .i32_const => |i| try std.leb.writeILEB128(e.writer, i),
                .i64_const => |i| try std.leb.writeILEB128(e.writer, i),
                .f32_const => |f| try e.writer.writeIntNative(u32, @bitCast(u32, @floatCast(f32, f))),
                .f64_const => |f| try e.writer.writeIntNative(u64, @bitCast(u64, f)),
                .global_get => |u| try e.uleb(u),
            }
            try e.byte(std.wasm.opcode(.end));
        }
        fn functype(e: E, sig: IR.Func.Sig) !void {
            try e.byte(std.wasm.function_type);
            try e.uleb(sig.params.len);
            for (sig.params) |param|
                try e.byte(std.wasm.valtype(param.lower()));
            try e.uleb(sig.results.len);
            for (sig.results) |ret|
                try e.byte(std.wasm.valtype(ret.lower()));
        }

        fn typeSection(e: E) !void {
            //MAYBE: deduplicate
            //NOTE: reftype not handled...
            if (e.m.funcs.len == 0) return;

            var len: usize = 0;
            for (e.m.funcs) |func| {
                len += 1 + switch (func.body) {
                    .code => |code| code.types.len,
                    .import => 0,
                };
            }
            try e.uleb(len);
            for (e.m.funcs) |func| {
                try e.functype(func.type);
                switch (func.body) {
                    .code => |code| for (code.types) |sig|
                        try e.functype(sig),
                    .import => {},
                }
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
            for (e.m.funcs) |func| if (func.body == .code) {
                len += 1;
            };
            if (len == 0) return;

            var typeidx: usize = 0;
            try e.uleb(len);
            for (e.m.funcs) |func| {
                switch (func.body) {
                    .code => |code| {
                        try e.uleb(typeidx);
                        typeidx += code.types.len;
                    },
                    else => {},
                }
                typeidx += 1;
            }
        }
        fn tableSection(e: E) !void {
            var len: usize = 0;
            for (e.m.tables) |table| if (table.body == .intern) {
                len += 1;
            };
            if (len == 0) return;

            try e.uleb(len);
            for (e.m.tables) |table| if (table.body == .intern) {
                try e.byte(std.wasm.reftype(table.type));
                try e.limits(table.size);
            };
        }
        fn memorySection(e: E) !void {
            if (e.m.memory) |mem| if (mem.import == null) {
                try e.uleb(1);
                try e.limits(mem.size);
            };
        }
        fn globalSection(e: E) !void {
            var len: usize = 0;
            for (e.m.globals) |global| if (global.body == .init) {
                len += 1;
            };
            if (len == 0) return;

            try e.uleb(len);
            for (e.m.globals) |global| switch (global.body) {
                .init => |init| {
                    try e.byte(std.wasm.valtype(global.type.lower()));
                    try e.byte(@boolToInt(global.mutable));
                    try e.constExpr(init);
                },
                else => {},
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
        fn startSection(e: E) !void {
            if (e.m.start) |s|
                try e.uleb(s);
        }
        fn elementSection(e: E) !void {
            if (e.m.elements.len == 0) return;

            try e.uleb(e.m.elements.len);
            for (e.m.elements) |elem| {
                var flag: u8 = switch (elem.mode) {
                    .passive => 0x1,
                    .declarative => 0x3,
                    .active => |act| if (act.table == 0) @as(u8, 0x0) else 0x2,
                };
                if (elem.init == .val) flag |= 0b100;
                try e.byte(flag);
                switch (elem.mode) {
                    .active => |act| {
                        if (act.table != 0)
                            try e.uleb(act.table);
                        try e.constExpr(act.offset);
                    },
                    else => {},
                }
                if (elem.mode != .active or elem.mode.active.table != 0)
                    try e.byte(@enumToInt(elem.type));
                switch (elem.init) {
                    .val => |vs| {
                        try e.uleb(vs.len);
                        for (vs) |v|
                            try e.constExpr(v);
                    },
                    .func => |vs| {
                        try e.uleb(vs.len);
                        for (vs) |v|
                            try e.uleb(v);
                    },
                }
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

            var typeidx: usize = 0;
            try e.uleb(len);
            for (e.m.funcs) |func| {
                typeidx += 1;
                switch (func.body) {
                    .code => |code| {
                        try e.uleb(code.bytes.len);
                        // NOTE: expect relocs stored by offset and not overlaping
                        var sent: u32 = 0;
                        for (code.relocs) |reloc| {
                            try e.writer.writeAll(code.bytes[sent..reloc.offset]);
                            sent = reloc.offset;
                            switch (reloc.type) {
                                .typeIndexLeb => {
                                    const v = @truncate(u32, typeidx + reloc.index);
                                    var buf: [5]u8 = undefined;
                                    std.leb.writeUnsignedFixed(5, &buf, v);
                                    try e.writer.writeAll(&buf);
                                    sent += 5;
                                },
                                else => unreachable,
                            }
                        }
                        try e.writer.writeAll(code.bytes[sent..]);
                        typeidx += code.types.len;
                    },
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
                        try e.constExpr(act.offset);
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
