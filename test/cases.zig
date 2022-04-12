const std = @import("std");

const tests = &[_]Test{
    .{ .success = false },
    .{ .args = &[_][]const u8{"help"} },
    Test.help(.parse),
    Test.hello(.parse, null),
    Test.help(.build),
    Test.hello(.build, null),
    Test.hello(.build, &[_][]const u8{"--format=compact"}),
    Test.ok(.build, "samples/fizzbuzz.wala", &[_][]const u8{"--format=compact"}),
    Test.help(.run),
    Test.hello(.run, null),
    Test.ok(.run, "samples/hello.wat", null),
    Test.ok(.run, "test/flat/hello.wat", null),
    Test.ok(.run, "test/hello.wasm", null),
    Test.ok(.run, "samples/fib.wala", &[_][]const u8{ "--", "--invoke", "fib", "20" }),
    Test.ok(.run, "test/flat/fib.wat", &[_][]const u8{ "--", "--invoke", "fib", "20" }),
    Test.ok(.run, "samples/gcd.wala", &[_][]const u8{ "--", "--invoke", "fib", "132", "56" }),
    Test.ok(.run, "samples/gcd.wat", &[_][]const u8{ "--", "--invoke", "fib", "132", "56" }),
    Test.ok(.run, "samples/99-bottles-of-beer.wala", null),
    Test.ok(.run, "samples/fizzbuzz.wala", null),
    Test.errTest(.parse, "fileNotFound.wat", null),
    Test.errTest(.parse, "topLevelIndent.wala", null),
    Test.errTest(.parse, "indentMismatch.wala", null),
    Test.errTest(.build, "typeMismatch.wat", null),
    Test.ok(.build, "test/custom/importMem.wat", null),
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const argv = try std.process.argsAlloc(gpa.allocator());
    defer std.process.argsFree(gpa.allocator(), argv);
    const run_cmd = argv[1..];

    var progress: std.Progress = .{};
    const root_node = try progress.start("Test", tests.len);

    for (tests) |task| {
        const test_name = try task.name(gpa.allocator());
        defer gpa.allocator().free(test_name);

        var test_node = root_node.start(test_name, 0);
        test_node.activate();
        progress.maybeRefresh();

        try task.run(run_cmd, gpa.allocator());
        test_node.end();
    }
    root_node.end();
    std.debug.print("All {} cases passed.\n", .{tests.len});
}

const Test = struct {
    const Do = enum { parse, build, run };
    do: ?Do = null,
    on: ?[]const u8 = null,
    args: []const []const u8 = &[_][]u8{},
    success: bool = true,

    fn help(do: Do) Test {
        return ok(do, "--help", null);
    }
    fn hello(do: Do, args: ?[]const []const u8) Test {
        return ok(do, "samples/hello.wala", args);
    }
    fn ok(do: Do, on: []const u8, args: ?[]const []const u8) Test {
        return .{ .do = do, .on = on, .args = args orelse &[_][]u8{} };
    }
    fn errTest(do: Do, comptime with: []const u8, args: ?[]const []const u8) Test {
        return .{ .success = false, .do = do, .on = "samples/error/" ++ with, .args = args orelse &[_][]u8{} };
    }

    fn name(self: Test, allocator: std.mem.Allocator) ![]const u8 {
        var args = std.ArrayList(u8).init(allocator);

        if (self.do) |do| {
            try args.appendSlice(@tagName(do));
            try args.append(' ');
        }
        if (self.on) |on| {
            try args.appendSlice(on);
            try args.append(' ');
        }
        for (self.args) |arg| {
            try args.appendSlice(arg);
            try args.append(' ');
        }

        return args.toOwnedSlice();
    }

    fn run(self: Test, cmd: []const []const u8, allocator: std.mem.Allocator) !void {
        var args = std.ArrayList([]const u8).init(allocator);
        defer args.deinit();

        try args.appendSlice(cmd);
        if (self.do) |do| try args.append(@tagName(do));
        if (self.on) |on| try args.append(on);
        try args.appendSlice(self.args);

        const child = try std.ChildProcess.init(args.items, allocator);
        defer child.deinit();

        child.stdout_behavior = .Pipe;
        child.stderr_behavior = .Pipe;

        const term = try child.spawnAndWait();
        const exit_code = switch (term) {
            .Exited => |code| code,
            else => return error.NotExited,
        };
        if (exit_code != @boolToInt(!self.success))
            return error.BadExitCode;

        //TODO: expected Stdio
        //print out & err
    }
};
