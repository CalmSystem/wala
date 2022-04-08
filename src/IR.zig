const u = @import("util.zig");
const std = @import("std");
pub const Linking = @import("IR/Linking.zig");
const iters = @import("IR/iters.zig");

pub const ulen = u32;

pub const Module = struct {
    arena: std.heap.ArenaAllocator,

    funcs: []const Func = &[_]Func{},
    tables: []const Table = &[_]Table{},
    memory: ?Memory = null,
    globals: []const Global = &[_]Global{},
    start: ?ulen = null,
    elements: []const Elem = &[_]Elem{},
    datas: []const Data = &[_]Data{},

    //linking: Linking,
    customs: []const Section.Custom = &[_]Section.Custom{},

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
};

pub const Numtype = std.wasm.Valtype;
pub const Valtype = Numtype;
/// Interface type proposal definitions
/// as https://github.com/WebAssembly/interface-types
pub const Intertype = union(enum) {
    f32,
    f64,
    s8,
    u8,
    s16,
    u16,
    s32,
    u32,
    s64,
    u64,
    char,
    list: *const Intertype,
    /// like struct
    record: []const Field,
    /// like union
    variant: []const Field,

    pub const Field = struct {
        name: u.Txt,
        id: ?u.Txt,
        type: *const Intertype,
    };

    pub const Abbrv = union(enum) {
        /// (list char)
        string,
        /// (record ("i" <intertype>)*) for i
        tuple: []const Intertype,
        /// (record (<name> bool))
        flags: []const u.Txt,
        /// (variant ("false") ("true"))
        bool,
        /// (variant (<name>)*)
        @"enum": []const u.Txt,
        /// (variant ("none") ("some" <intertype))
        option: Intertype,
        /// (variant ("i" <intertype>)*) for i
        @"union": []const Intertype,
        /// (variant ("ok" <intertype>?) ("error" <intertype>?))
        expected: struct {
            ok: ?Intertype,
            err: ?Intertype,
        },
    };
};

/// Internal signature types
pub const Sigtype = enum {
    i32,
    i64,
    f32,
    f64,
    s8,
    u8,
    s16,
    u16,
    s32,
    u32,
    s64,
    u64,

    pub fn lower(it: Sigtype) Numtype {
        return switch (it) {
            .i32, .s8, .u8, .s16, .u16, .s32, .u32 => .i32,
            .i64, .s64, .u64 => .i64,
            .f32 => .f32,
            .f64 => .f64,
        };
    }
    pub fn eql(a: Sigtype, b: Sigtype) bool {
        return @enumToInt(a) == @enumToInt(b);
    }
};

pub const Func = struct {
    body: union(enum) {
        import: ImportName,
        code: Code,
    },
    id: ?u.Txt,
    exports: []const ExportName = &[_]ExportName{},
    type: Sig,

    pub const Sig = struct {
        params: []const Sigtype = &[_]Sigtype{},
        results: []const Sigtype = &[_]Sigtype{},
    };
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
    type: Sigtype,
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

pub const Code = struct {
    bytes: u.Bin,
    /// block types (indexed as Reloc .typeIndexLeb)
    types: []const Func.Sig = &[_]Func.Sig{},
    relocs: []const Linking.Reloc.Entry = &[_]Linking.Reloc.Entry{},

    pub const Op = std.wasm.Opcode;
    pub const MemArg = struct {
        align_: u32,
        offset: u32 = 0,
    };
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
        relocs: []const Linking.Reloc.Entry = &[_]Linking.Reloc.Entry{},
    };
};
