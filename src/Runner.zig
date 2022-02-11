const std = @import("std");
const Parser = @import("Parser.zig");
const Reader = @import("Reader.zig");
const Context = @import("Context.zig");

inline fn strEql(a: []const u8, b: Parser.Name) bool {
    return std.mem.eql(u8, a, b);
}

const Func = struct {
    //FIXME: assume one argument
    arg: ?Parser.Name = null,
    body: Parser.Block,

    fn call(func: Func, self: *Self, arg: ?Value) Error!Value {
        var scope = self.child();
        defer scope.deinit();

        std.debug.assert((func.arg != null) == (arg != null));
        if (func.arg != null or arg != null)
            try scope.set(func.arg.?, arg.?);

        var i: usize = 0;
        while (i < func.body.len) : (i += 1) {
            const val = try scope.eval(func.body[i]);
            try scope.set(Context.PIPELINE, val);
        }
        return try scope.get(Context.PIPELINE);
    }
};
const Value = union(enum) {
    func: Func,
    int: i128,
    float: f128,
    boolean: bool,
    name: Parser.Name,
    void: void,

    fn eval(v: Value, self: *Self) Error!Value {
        return switch (v) {
            .name => |name| try self.get(name),
            .func => |func| if (func.arg == null)
                try func.call(self, null)
            else
                v,
            else => v,
        };
    }
};
const Var = struct {
    name: Parser.Name,
    value: Value,
};
const Scope = std.ArrayList(Var);

const Self = @This();

scope: Scope,
parent: ?*Self,

pub inline fn init(gpa: std.mem.Allocator) Self {
    return Self{ .scope = Scope.init(gpa), .parent = null };
}
pub inline fn deinit(self: Self) void {
    self.scope.deinit();
}

fn child(self: *Self) Self {
    return Self{ .scope = Scope.init(self.scope.allocator), .parent = self };
}
inline fn set(self: *Self, name: Parser.Name, value: Value) !void {
    try self.scope.append(Var{ .name = name, .value = value });
}
fn find(self: Self, name: Parser.Name) ?Value {
    var i: usize = 0;
    const s = self.scope.items;
    while (i < s.len) : (i += 1) {
        if (strEql(s[i].name, name))
            return s[i].value;
    }
    return if (self.parent) |p| p.find(name) else null;
}
inline fn get(self: Self, name: Parser.Name) !Value {
    return self.find(name) orelse return Error.UnknownVariable;
}

pub fn runOut(self: *Self, file: Reader.Parsed, comptime Emitter: type, emitter: Emitter) !void {
    var i: usize = 0;
    while (i < file.tree.len) : (i += 1)
        try emitter.out(try self.eval(file.tree[i]));
}
pub fn run(self: *Self, file: Reader.Parsed) !void {
    const Dummy = struct {
        fn out(_: @This(), _: Value) !void {}
    };
    try self.runOut(file, Dummy, .{});
}
pub fn runLog(self: *Self, file: Reader.Parsed, comptime Writer: type, writer: Writer) !void {
    const Adaptor = struct {
        writer: Writer,
        fn out(adaptor: @This(), value: Value) !void {
            const w = adaptor.writer;
            try switch (value) {
                .int => |int| w.print("{}", .{int}),
                .float => |float| w.print("{}", .{float}),
                .boolean => |boolean| w.print("{}", .{boolean}),
                .name => |name| w.print("\"{s}\"n.", .{name}),
                .func => |func| w.print("{s} -> @{}", .{ func.arg, func.body[0].at.offset }),
                .void => w.print(".", .{}),
            };
            try w.writeAll("\n");
        }
    };
    try self.runOut(file, Adaptor, Adaptor{ .writer = writer });
}

const Error = error{
    /// Calling undefined method
    MissingMethod,
    /// Calling undefined variable
    UnknownVariable,
    UnhandledNodeKind,
    /// Cannot print
    PrintFailed,
} || std.mem.Allocator.Error;
fn eval(self: *Self, node: Parser.Node) Error!Value {
    return switch (node.value) {
        .name => |name| Value{ .name = name },
        .integer => |int| Value{ .int = int },
        .decimal => |float| Value{ .float = float },
        .tuple => |tuple| {
            if (tuple.len == 0)
                return Value{ .void = .{} };
            std.debug.assert(tuple.len == 1);
            return self.eval(tuple[0]);
        },
        .block => |block| Value{ .func = Func{ .body = block } },
        .call => |call| {
            const on = try self.eval(call.on.*);
            const arg = try self.eval(call.arg.*);
            return self.call(on, arg, call.method);
        },
    };
}

fn print(value: anytype) Error!void {
    const format = if (@TypeOf(value) == []const u8) "{s}\n" else "{}\n";
    std.io.getStdOut().writer().print(format, .{value}) catch return Error.PrintFailed;
}
fn call(self: *Self, on: Value, arg: Value, do: Parser.Name) Error!Value {
    return switch (on) {
        .int => |int| {
            if (strEql(Context.PRINT, do)) {
                try print(int);
                return on;
            }
            std.debug.assert(do.len == 1);
            return switch (do[0]) {
                '+' => Value{ .int = int + arg.int },
                '*' => Value{ .int = int * arg.int },
                '-' => Value{ .int = int - arg.int },
                '<' => Value{ .boolean = int < arg.int },
                '>' => Value{ .boolean = int > arg.int },
                ':' => on,
                else => {
                    std.log.err("{s}", .{do});
                    return Error.MissingMethod;
                },
            };
        },
        .float => |float| {
            if (strEql(Context.PRINT, do)) {
                try print(float);
                return on;
            }
            std.debug.assert(do.len == 1);
            return Value{ .float = switch (do[0]) {
                '+' => float + arg.float,
                '*' => float * arg.float,
                '-' => float - arg.float,
                else => {
                    std.log.err("{s}", .{do});
                    return Error.MissingMethod;
                },
            } };
        },
        .boolean => |b| {
            if (strEql(Context.PRINT, do)) {
                try print(b);
                return on;
            }
            std.debug.assert(do.len == 1);
            return switch (do[0]) {
                '?' => if (b) arg.eval(self) else Value{ .boolean = false },
                ':' => if (!b) arg.eval(self) else on,
                else => {
                    std.log.err("{s}", .{do});
                    return Error.MissingMethod;
                },
            };
        },
        .name => |name| {
            if (strEql(Context.PRINT, do)) {
                try print(name);
                return on;
            } else if (strEql(Context.ASSIGN, do)) {
                try self.set(name, arg);
                return arg;
            } else if (strEql(Context.CALL, do)) {
                const func = (try self.get(name)).func;
                return try func.call(self, arg);
            } else if (strEql(Context.FUNCTION, do)) {
                std.debug.assert(arg.func.arg == null);
                return Value{ .func = Func{ .arg = on.name, .body = arg.func.body } };
            } else {
                return try self.call(try self.get(name), arg, do);
            }
        },
        else => Error.UnhandledNodeKind,
    };
}
