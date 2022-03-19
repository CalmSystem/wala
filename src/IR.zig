const u = @import("util.zig");
const std = @import("std");
pub const Linking = @import("IR/Linking.zig");
const iters = @import("IR/iters.zig");

pub const ulen = u32;

pub const Module = struct {
    arena: std.heap.ArenaAllocator,

    funcs: []Func = &[_]Func{},
    tables: []Table = &[_]Table{},
    memory: ?Memory = null,
    globals: []Global = &[_]Global{},
    start: ?ulen = null,
    elements: []Elem = &[_]Elem{},
    datas: []Data = &[_]Data{},

    //linking: Linking,
    customs: []Section.Custom = &[_]Section.Custom{},

    pub fn init(allocator: std.mem.Allocator) Module {
        return .{ .arena = std.heap.ArenaAllocator.init(allocator) };
    }
    pub fn deinit(self: Module) void {
        self.arena.deinit();
    }

    pub fn link(_: []const Module, allocator: std.mem.Allocator) !Module {
        var m = init(allocator);
        //TODO:
        return m;
    }

    pub fn imports(self: *const Module) iters.Import {
        return .{ .m = self };
    }
    pub fn exports(self: *const Module) iters.Export {
        return .{ .m = self };
    }

    pub fn findFunc(self: *const Module, id: u.Txt) ?FoundId(Func) {
        return findById(Func, self.funcs, id);
    }
    pub fn findGlobal(self: *const Module, id: u.Txt) ?FoundId(Global) {
        return findById(Global, self.globals, id);
    }
    pub fn FoundId(comptime T: type) type {
        return struct { ptr: *const T, idx: u32 };
    }
    fn findById(comptime T: type, list: []const T, id: u.Txt) ?FoundId(T) {
        return for (list) |*t, i| {
            if (t.id != null and u.strEql(id, t.id.?))
                break .{ .ptr = t, .idx = @truncate(ulen, i) };
        } else null;
    }
};

pub const Func = struct {
    body: union(enum) {
        import: ImportName,
        code: Code,
    },
    id: ?u.Txt,
    exports: []const ExportName = &[_]ExportName{},
    type: Type,

    /// valtypes in arena
    pub const Type = std.wasm.Type;
    pub const valtype = std.wasm.valtype;
    pub const Valtype = std.wasm.Valtype;
};

pub const Table = struct {
    body: union(enum) { import: ImportName, intern: void },
    id: ?u.Txt,
    exports: []const ExportName = &[_]ExportName{},
    type: std.wasm.RefType,
    size: std.wasm.Limits,
};

pub const Memory = struct {
    import: ?ImportName = null,
    id: ?u.Txt,
    exports: []const ExportName = &[_]ExportName{},
    size: std.wasm.Limits,
};

pub const Global = struct {
    body: union(enum) {
        import: ImportName,
        code: Code,
    },
    id: ?u.Txt,
    exports: []const ExportName = &[_]ExportName{},
    type: Func.Valtype,
    mutable: bool,
};

pub const Elem = struct {
    //TODO:
};

pub const Data = struct {
    body: union(enum) {
        active: struct {
            mem: u32,
            offset: Code,
            content: u.Bin,
        },
        passive: u.Bin,
    },
    id: ?u.Txt,
};

pub const Opcode = std.wasm.Opcode;
pub const opcode = std.wasm.opcode;
pub const Code = struct {
    bytes: u.Bin,
    relocs: []Linking.Reloc.Entry = &[_]Linking.Reloc.Entry{},
};

pub const ImportName = struct {
    module: u.Txt,
    name: u.Txt,
};
pub const ExportName = u.Txt;

pub const Section = struct {
    pub const Type = std.wasm.Section;

    pub const Custom = struct {
        name: u.Txt,
        content: u.Bin,
        after: union(enum) {
            common: Type,
            custom: u.Txt,
        },
        relocs: []Linking.Reloc.Entry = &[_]Linking.Reloc.Entry{},
    };
};
