const std = @import("std");

pub const ulen = u32;
pub const Valtype = std.wasm.Valtype;
pub const FuncType = std.wasm.Type;

const Func = struct {
    key: []const u8 = "",
    type_index: ulen,
    code: []const u8,

    pub fn defined(a: Func) bool {
        return a.code.len > 0;
    }

    pub fn key_eql(a: Func, key: []const u8) bool {
        return a.key.len > 0 and std.mem.eql(u8, a.key, key);
    }
    pub fn eql(a: Func, b: Func) bool {
        return a.key_eql(b.key);
    }
};
const Memory = struct {
    data_end: ulen = 0,
    datas: std.ArrayListUnmanaged(Data) = .{},

    const Data = struct {
        offset: ulen,
        bytes: []const u8,
    };

    pub fn deinit(self: *Memory, alloc: std.mem.Allocator) void {
        self.datas.deinit(alloc);
    }
};

pub const FuncIdx = union(enum) {
    import: ulen,
    local: ulen,

    pub inline fn global(self: FuncIdx, imports: ulen) ulen {
        return switch (self) {
            .import => |idx| idx,
            .local => |idx| imports + idx,
        };
    }
};
const Export = struct {
    name: []const u8,
    value: Kind,

    const Kind = union(std.wasm.ExternalKind) {
        function: FuncIdx,
        table: ulen,
        memory: ulen,
        global: ulen,
    };

    pub fn eql(a: Export, b: Export) bool {
        return std.mem.eql(u8, a.name, b.name);
    }
};
const Import = struct {
    module: []const u8,
    name: []const u8,
    kind: Kind,

    pub const Kind = union(std.wasm.ExternalKind) {
        function: ulen,
        table: void, //TODO:
        memory: void,
        global: void,
    };

    pub fn eql(a: Import, b: Import) bool {
        return @enumToInt(a.kind) == @enumToInt(b.kind) and
            std.mem.eql(u8, a.module, b.module) and
            std.mem.eql(u8, a.name, b.name);
    }
};

arena: std.heap.ArenaAllocator,

types: UniqueIndexUnmanaged(FuncType) = .{},
funcs: UniqueIndexUnmanaged(Func) = .{},
//MAYBE: more memories
mem0: Memory = .{},
//MAYBE: passive data

imports: UniqueIndexUnmanaged(Import) = .{},
imports_frozen: bool = false,
exports: UniqueIndexUnmanaged(Export) = .{},

const Self = @This();

inline fn allocator(self: Self) std.mem.Allocator {
    return self.arena.child_allocator;
}

pub fn init(alloc: std.mem.Allocator) Self {
    return Self{ .arena = std.heap.ArenaAllocator.init(alloc) };
}

pub fn deinit(self: *Self) void {
    self.types.deinit(self.allocator());
    self.funcs.deinit(self.allocator());
    self.mem0.deinit(self.allocator());

    self.imports.deinit(self.allocator());
    self.exports.deinit(self.allocator());

    self.arena.deinit();
}

pub const emptyValtypes = &[_]Valtype{};
pub fn createValtypes(self: Self, n: ulen) ![]Valtype {
    return try self.arena.allocator().alloc(Valtype, n);
}
const i32s: []const Valtype = &[_]Valtype{Valtype.i32} ** 16;
pub fn i32Valtypes(n: ulen) []const Valtype {
    std.debug.assert(n <= i32s.len);
    return i32s[0..n];
}

pub fn defineFunc(self: Self, idx: FuncIdx, code: []const u8) void {
    const func = &self.funcs.items[idx.local];
    std.debug.assert(!func.defined());
    func.code = code;
}

pub fn addType(self: *Self, typ: FuncType) !ulen {
    return try self.types.put(typ, self.allocator());
}
pub fn addFuncImport(self: *Self, module: []const u8, name: []const u8, typ: FuncType) !FuncIdx {
    std.debug.assert(!self.imports_frozen);
    return FuncIdx{ .import = try self.imports.put(.{ .module = module, .name = name, .kind = .{ .function = try self.addType(typ) } }, self.allocator()) };
}
const AddFuncOpt = struct {
    key: []const u8 = "",
    code: []const u8 = "",
};
pub fn addFunc(self: *Self, typ: FuncType, opt: AddFuncOpt) !FuncIdx {
    return FuncIdx{ .local = try self.funcs.put(.{ .type_index = try self.addType(typ), .key = opt.key, .code = opt.code }, self.allocator()) };
}
pub fn addData(self: *Self, bytes: []const u8, comptime unique: bool) !ulen {
    if (!unique) {
        var i: ulen = 0;
        const items = self.mem0.datas.items;
        while (i < items.len) : (i += 1) {
            if (std.mem.eql(u8, bytes, items[i].bytes))
                return items[i].offset;
        }
    }
    const offset = self.mem0.data_end;
    try self.mem0.datas.append(self.allocator(), .{ .offset = offset, .bytes = bytes });
    self.mem0.data_end += @truncate(ulen, bytes.len);
    return offset;
}
pub fn addDataZeros(self: *Self, size: ulen) ulen {
    const offset = self.mem0.data_end;
    self.mem0.data_end += size;
    return offset;
}
pub fn addExport(self: *Self, name: []const u8, value: Export.Kind) !void {
    _ = try self.exports.put(.{ .name = name, .value = value }, self.allocator());
}
pub inline fn exportMemory(self: *Self) !void {
    try self.addExport("memory", .{ .memory = 0 });
}

pub fn findType(self: Self, idx: ulen) ?FuncType {
    return if (idx < self.types.items.len)
        self.types.items[idx] else null;
}
const FuncInfo = struct {
    idx: FuncIdx,
    type_index: ulen,
};
pub fn findFunc(self: Self, key: []const u8) ?FuncInfo {
    return if (self.funcs.find(.{ .type_index = 0, .key = key })) |idx|
        .{ .idx = .{ .local=idx }, .type_index = self.funcs.items[idx].type_index } else null;
}
pub fn findFuncImport(self: Self, module: []const u8, name: []const u8) ?FuncInfo {
    return if (self.import.find(.{ .kind = .{ .function = 0 }, .module = module, .name = name })) |idx|
        .{ .idx = .{ .local=idx }, .type_index = self.funcs.items[idx].type_index } else null;
}

const CodeWriter = struct {
    code: std.ArrayList(u8),
    imports: ulen,

    pub fn init(alloc: std.mem.Allocator, params: ulen, locals: []const Valtype, imports: ulen) !CodeWriter {
        var self = CodeWriter{ .code = std.ArrayList(u8).init(alloc), .imports = imports };

        try self.uleb(locals.len);
        var i: ulen = 0;
        while (i < locals.len) : (i += 1) {
            try self.uleb(params + i);
            try self.byte(std.wasm.valtype(locals[i]));
        }

        return self;
    }
    pub fn deinit(self: CodeWriter) void {
        self.code.deinit();
    }

    pub const OpCode = std.wasm.Opcode;
    const MemArg = struct {
        align_: u32 = 2,
        offset: u32 = 0,
    };

    fn byte(self: *CodeWriter, b: u8) !void {
        try self.code.append(b);
    }
    inline fn opcode(self: *CodeWriter, op: OpCode) !void {
        try self.byte(std.wasm.opcode(op));
    }
    fn uleb(self: *CodeWriter, v: u64) !void {
        try std.leb.writeULEB128(self.code.writer(), v);
    }
    fn ileb(self: *CodeWriter, v: i64) !void {
        try std.leb.writeILEB128(self.code.writer(), v);
    }

    pub inline fn i32_(self: *CodeWriter, v: i32) !void {
        try self.opcode(OpCode.i32_const);
        try self.ileb(v);
    }
    pub inline fn i32_store(self: *CodeWriter, m: MemArg) !void {
        try self.opcode(OpCode.i32_store);
        try self.uleb(m.align_);
        try self.uleb(m.offset);
    }

    pub inline fn call_raw(self: *CodeWriter, idx: ulen) !void {
        try self.opcode(OpCode.call);
        try self.uleb(idx);
    }
    pub inline fn call(self: *CodeWriter, idx: FuncIdx) !void {
        try self.call_raw(idx.global(self.imports));
    }
    pub inline fn drop(self: *CodeWriter) !void {
        try self.opcode(OpCode.drop);
    }
    pub inline fn end(self: *CodeWriter) !void {
        try self.opcode(OpCode.end);
    }

    pub fn finish(self: *CodeWriter) ![]const u8 {
        if (self.code.items[self.code.items.len - 1] != std.wasm.opcode(OpCode.end))
            try self.end();
        return self.code.items;
    }
};
pub fn codeWriter(self: *Self, params: ulen, locals: []const Valtype) !CodeWriter {
    self.imports_frozen = true;
    return try CodeWriter.init(self.allocator(), params, locals, self.imports.size());
}
pub fn allocBytes(self: *Self, bytes: []const u8) ![]const u8 {
    return try self.arena.allocator().dupe(u8, bytes);
}

fn UniqueIndexUnmanaged(comptime T: type) type {
    return struct {
        items: []T = &[_]T{},
        capacity: usize = 0,

        const It = @This();

        pub fn deinit(self: *It, alloc: std.mem.Allocator) void {
            alloc.free(self.allocatedSlice());
            self.* = undefined;
        }
        pub fn allocatedSlice(self: It) []T {
            return self.items.ptr[0..self.capacity];
        }
        pub fn find(self: It, v: T) ?ulen {
            var i: ulen = 0;
            while (i < self.items.len) : (i += 1) {
                if (v.eql(self.items[i]))
                    return i;
            }
            return null;
        }
        pub fn put(self: *It, v: T, alloc: std.mem.Allocator) !ulen {
            if (self.find(v)) |i| return i;
            if (self.items.len == self.capacity) {
                const new_capacity = if (self.capacity == 0) 16 else self.capacity * 4;
                const new_mem = try alloc.realloc(self.allocatedSlice(), new_capacity);
                self.items.ptr = new_mem.ptr;
                self.capacity = new_capacity;
            }
            const i = self.items.len;
            self.items.len += 1;
            self.items[i] = v;
            return @truncate(ulen, i);
        }
        pub inline fn size(self: It) ulen {
            return @truncate(ulen, self.items.len);
        }
    };
}

fn Emitter(comptime Writer: type) type {
    return struct {
        writer: Writer,
        ctx: Self,

        const E = @This();
        fn typeSection(e: E) !void {
            //NOTE: reftype not handled...
            const items = e.ctx.types.items;
            if (items.len == 0) return;

            try e.uleb(items.len);
            for (items) |item| {
                try e.writer.writeByte(std.wasm.function_type);
                try e.uleb(item.params.len);
                for (item.params) |param|
                    try e.writer.writeByte(std.wasm.valtype(param));
                try e.uleb(item.returns.len);
                for (item.returns) |ret|
                    try e.writer.writeByte(std.wasm.valtype(ret));
            }
        }
        fn importSection(e: E) !void {
            const items = e.ctx.imports.items;
            if (items.len == 0) return;

            try e.uleb(items.len);
            for (items) |item| {
                try e.uleb(item.module.len);
                try e.writer.writeAll(item.module);
                try e.uleb(item.name.len);
                try e.writer.writeAll(item.name);
                try e.writer.writeByte(@enumToInt(item.kind));
                switch (item.kind) {
                    .function => |idx| try e.uleb(idx),
                    else => @panic("TODO: unimplemented"),
                }
            }
        }
        fn functionSection(e: E) !void {
            const items = e.ctx.funcs.items;
            if (items.len == 0) return;

            try e.uleb(items.len);
            for (items) |item|
                try e.uleb(item.type_index);
        }
        fn memorySection(e: E) !void {
            try e.uleb(1);
            const pages = (e.ctx.mem0.data_end / std.wasm.page_size) + 1;
            try e.writer.writeByte(0);
            try e.uleb(pages);
        }
        fn exportSection(e: E) !void {
            const items = e.ctx.exports.items;
            if (items.len == 0) return;

            const imports = @truncate(ulen, e.ctx.imports.items.len);
            try e.uleb(items.len);
            for (items) |item| {
                try e.uleb(item.name.len);
                try e.writer.writeAll(item.name);
                try e.writer.writeByte(@enumToInt(item.value));
                try e.uleb(switch (item.value) {
                    .function => |f| f.global(imports),
                    .table, .memory, .global => |idx| idx,
                });
            }
        }
        fn codeSection(e: E) !void {
            const items = e.ctx.funcs.items;
            if (items.len == 0) return;

            try e.uleb(items.len);
            for (items) |item| {
                std.debug.assert(item.defined());
                try e.uleb(item.code.len);
                try e.writer.writeAll(item.code);
            }
        }
        fn dataSection(e: E) !void {
            const items = e.ctx.mem0.datas.items;
            if (items.len == 0) return;

            try e.uleb(items.len);
            for (items) |item| {
                try e.writer.writeByte(0);
                try e.writer.writeByte(std.wasm.opcode(std.wasm.Opcode.i32_const));
                try e.uleb(item.offset);
                try e.writer.writeByte(std.wasm.opcode(std.wasm.Opcode.end));
                try e.uleb(item.bytes.len);
                try e.writer.writeAll(item.bytes);
            }
        }

        pub fn section(e: E, comptime kind: std.wasm.Section) !void {
            // A bit too meta...
            const fn_name = @tagName(kind) ++ "Section";

            var size: ulen = 0;
            const se = Emitter(SizeWriter){ .ctx = e.ctx, .writer = .{ .context = .{ .size = &size } } };
            try @field(se, fn_name)();
            if (size == 0) return;

            try e.writer.writeByte(std.wasm.section(kind));
            try e.uleb(size);
            try @field(e, fn_name)();
        }
        fn uleb(e: E, v: usize) !void {
            try std.leb.writeULEB128(e.writer, v);
        }
        fn ulebSize(value: usize) ulen {
            var size: ulen = 0;
            var v = value;
            while (true) {
                size += 1;
                v >>= 7;
                if (v == 0)
                    return size;
            }
        }
    };
}
const SizeContext = struct {
    size: *ulen,
    fn writeFn(self: SizeContext, bytes: []const u8) error{}!usize {
        self.size.* += @truncate(ulen, bytes.len);
        return bytes.len;
    }
};
const SizeWriter = std.io.Writer(SizeContext, error{}, SizeContext.writeFn);

pub fn emit(self: Self, comptime Writer: type, writer: Writer) !void {
    const e = Emitter(Writer){ .writer = writer, .ctx = self };

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
}
