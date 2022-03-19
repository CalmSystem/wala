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
                try e.string(@bitCast(u.Bin, func.type.params));
                // try e.uleb(func.type.params.len);
                // for (func.type.params) |param|
                //     try e.byte(std.wasm.valtype(param));
                try e.uleb(func.type.returns.len);
                for (func.type.returns) |ret|
                    try e.byte(std.wasm.valtype(ret));
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
                        try e.byte(std.wasm.valtype(g.type));
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
