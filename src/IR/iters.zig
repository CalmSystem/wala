const std = @import("std");
const IR = @import("../IR.zig");

pub const External = std.wasm.ExternalKind;
inline fn nextExternal(e: External) External {
    if (e == .global) return e;
    return @intToEnum(External, @enumToInt(e)+1);
}

pub const Import = struct {
    m: *const IR.Module,
    state: External = .function,
    offset: usize = 0,

    const Point = struct {
        key: IR.ImportName,
        kind: External,
        index: usize,
    };
    pub fn next(self: *Import) ?Point {
        return switch (self.state) {
            .function => self.nextN("funcs"),
            .table => self.nextN("tables"),
            .memory => if (self.offset == 0 and self.m.memory != null and self.m.memory.?.import != null) {
                self.offset += 1;
                return Point{ .key = self.m.memory.?.import.?, .kind = .memory, .index = 0 };
            } else {
                self.state = nextExternal(self.state);
                self.offset = 0;
                return self.next();
            },
            .global => self.nextN("globals")
        };
    }
    inline fn nextN(self: *Import, comptime fname: []const u8) ?Point {
        const slice = @field(self.m, fname);
        while (self.offset < slice.len) {
            const index = self.offset;
            const body = slice[index].body;
            self.offset += 1;
            switch (body) {
                .import => |key|
                    return Point{ .key = key, .kind = self.state, .index = index },
                else => {} 
            }
        }
        if (self.state == .global) return null;
        self.state = nextExternal(self.state);
        self.offset = 0;
        return self.next();
    }
};

pub const Export = struct {
    m: *const IR.Module,
    state: External = .function,
    offset: struct {
        field: usize = 0,
        expor: usize = 0,
    } = .{},

    const Point = struct {
        key: IR.ExportName,
        kind: External,
        index: usize,
    };
    pub fn next(self: *Export) ?Point {
        return switch (self.state) {
            .function => self.nextN("funcs"),
            .table => self.nextN("tables"),
            .memory => if (self.offset.field == 0 and self.m.memory != null and self.offset.expor < self.m.memory.?.exports.len) {
                const key = self.m.memory.?.exports[self.offset.expor];
                self.offset.expor += 1;
                return Point{ .key = key, .kind = .memory, .index = 0 };
            } else {
                self.state = nextExternal(self.state);
                self.offset = .{ };
                return self.next();
            },
            .global => self.nextN("globals")
        };
    }
    inline fn nextN(self: *Export, comptime fname: []const u8) ?Point {
        const slice = @field(self.m, fname);
        while (self.offset.field < slice.len) {
            const exps = slice[self.offset.field].exports;
            if (self.offset.expor < exps.len) {
                const key = exps[self.offset.expor];
                self.offset.expor += 1;
                return Point{ .key = key, .kind = self.state, .index = self.offset.field };
            }
            self.offset.expor = 0;
            self.offset.field += 1;
        }
        if (self.state == .global) return null;
        self.state = nextExternal(self.state);
        self.offset.field = 0;
        return self.next();
    }
};
