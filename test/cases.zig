const std = @import("std");

const tests = &[_]Test{
    .{ .success = false },
    .{ .args = &[_][]const u8{"help"} },
    Test.help(.parse),
    Test.hello(.parse, null, null),
    Test.help(.build),
    Test.hello(.build, null, null),
    Test.hello(.build, &[_][]const u8{"--format=compact"}, null),
    Test.ok(.build, "samples/fizzbuzz.wala", &[_][]const u8{"--format=compact"}, null),
    Test.help(.run),
    Test.hello(.run, null, "Hello, W0rld!\n"),
    Test.runSample("hello.wat", null, "Hello, W0rld!\n"),
    Test.runTestFlat("hello.wat", null, "Hello, W0rld!\n"),
    //FIXME: Test.ok(.run, "test/hello.wasm", null, "Hello, W0rld!\n"),
    Test.runSample("fib.wala", &[_][]const u8{ "--", "--invoke", "fib", "20" }, "6765\n"),
    Test.runTestFlat("fib.wat", &[_][]const u8{ "--", "--invoke", "fib", "20" }, "6765\n"),
    Test.runSample("gcd.wala", &[_][]const u8{ "--", "--invoke", "gcd", "132", "56" }, "4\n"),
    Test.runSample("gcd.wat", &[_][]const u8{ "--", "--invoke", "gcd", "132", "56" }, "4\n"),
    Test.runSample("pi.wala", &[_][]const u8{ "--", "--invoke", "pi", "1000000" }, "3.1415916535897743\n"),
    Test.runSampleContains("99-bottles-of-beer.wala", null, "1 bottle of beer."),
    Test.runSampleContains("fizzbuzz.wala", null, "Buzz Fizz 22"),
    Test.errTest(.parse, "FileNotFound"),
    Test.errTest(.parse, "TopLevelIndent"),
    Test.errTest(.parse, "IndentMismatch"),
    Test.errTest(.build, "TypeMismatch"),
    Test.ok(.build, "test/custom/importMem.wat", null, null),
};

pub fn main() void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const argv = std.process.argsAlloc(gpa.allocator()) catch unreachable;
    defer std.process.argsFree(gpa.allocator(), argv);
    const run_cmd = argv[1..];

    var n_ok: usize = 0;
    var progress: std.Progress = .{};
    const root_node = progress.start("Test", tests.len) catch unreachable;

    for (tests) |task| {
        const test_name = task.name(gpa.allocator()) catch unreachable;
        defer gpa.allocator().free(test_name);

        var test_node = root_node.start(test_name, 0);
        test_node.activate();
        progress.refresh();

        const result = task.run(run_cmd, gpa.allocator());
        test_node.end();
        if (result) |_| {
            n_ok += 1;
        } else |err| {
            progress.log("{s}... FAIL ({s})\n", .{ test_name, @errorName(err) });
            std.debug.dumpStackTrace(@errorReturnTrace().?.*);
        }
    }
    root_node.end();
    if (n_ok == tests.len) {
        std.debug.print("All {} cases passed.\n", .{tests.len});
    } else {
        std.debug.print("{} of {} cases passed.\n", .{ n_ok, tests.len });
        std.process.exit(1);
    }
}

const Test = struct {
    const Do = enum { parse, build, run };
    do: ?Do = null,
    on: ?[]const u8 = null,
    args: []const []const u8 = &[_][]u8{},
    success: bool = true,
    output: ?[]const u8 = null,
    output_match: enum { equals, contains } = .equals,

    inline fn help(do: Do) Test {
        return .{ .do = do, .on = "--help" };
    }
    inline fn hello(do: Do, args: ?[]const []const u8, output: ?[]const u8) Test {
        return ok(do, "samples/hello.wala", args, output);
    }
    inline fn ok(do: Do, comptime on: []const u8, args: ?[]const []const u8, output: ?[]const u8) Test {
        return .{ .do = do, .on = on, .args = args orelse &[_][]u8{}, .output = output };
    }
    inline fn runSample(comptime on: []const u8, args: ?[]const []const u8, output: ?[]const u8) Test {
        return ok(.run, "samples/" ++ on, args, output);
    }
    inline fn runSampleContains(comptime on: []const u8, args: ?[]const []const u8, output: ?[]const u8) Test {
        var t = ok(.run, "samples/" ++ on, args, output);
        t.output_match = .contains;
        return t;
    }
    inline fn runTestFlat(comptime on: []const u8, args: ?[]const []const u8, output: ?[]const u8) Test {
        return ok(.run, "test/flat/" ++ on, args, output);
    }
    inline fn errTest(do: Do, comptime with: []const u8) Test {
        return .{ .success = false, .do = do, .on = "test/error/" ++ with ++ ".wala", .output = "error." ++ with, .output_match = .contains };
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

        const result = try std.ChildProcess.exec(.{ .allocator = allocator, .argv = args.items });
        defer allocator.free(result.stdout);
        defer allocator.free(result.stderr);

        const exit_code = switch (result.term) {
            .Exited => |code| code,
            else => return error.NotExited,
        };
        if (exit_code != @boolToInt(!self.success))
            return error.BadExitCode;

        if (self.output) |expect| {
            const out = if (self.success) result.stdout else result.stderr;
            if (!switch (self.output_match) {
                .equals => std.mem.eql(u8, out, expect),
                .contains => std.mem.indexOf(u8, out, expect) != null,
            }) try std.testing.expectEqualStrings(expect, out);
        }
    }
};
