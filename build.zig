const std = @import("std");
const deps = @import("deps.zig");

pub fn build(b: *std.build.Builder) void {
    const target = b.standardTargetOptions(.{});
    const mode = b.standardReleaseOptions();

    const exe = b.addExecutable("wala", "src/main.zig");
    const exe_tests = b.addTest("src/main.zig");

    for (&[_]*std.build.LibExeObjStep{ exe, exe_tests }) |e| {
        e.setTarget(target);
        e.setBuildMode(mode);
        e.single_threaded = true;
        deps.addAllTo(e);
    }

    exe.install();

    const fmt = b.addFmt(&[_][]const u8{ "src", "build.zig" });
    const fmt_step = b.step("fmt", "Format all code");
    fmt_step.dependOn(&fmt.step);

    const run_cmd = exe.run();
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const coverage = b.option(bool, "coverage", "Generate coverage with kcov") orelse false;
    const kcov_args = &[_][]const u8{ "kcov", "--include-path=src", "--path-strip-level=1", b.getInstallPath(.prefix, "coverage") };

    if (coverage) {
        b.makePath(kcov_args[kcov_args.len - 1]) catch unreachable;
        var args: [kcov_args.len + 1]?[]const u8 = undefined;
        for (kcov_args) |arg, i| args[i] = arg;
        // to get zig to use the --test-cmd-bin flag
        args[kcov_args.len] = null;
        exe_tests.setExecCmd(&args);
    }
    const unit_test_step = b.step("unit-test", "Run unit tests");
    unit_test_step.dependOn(&exe_tests.step);

    const verify_tests = &[_]VerifyTest{
        .{ .success = false },
        .{ .args = &[_][]const u8{"help"} },
        VerifyTest.help(.parse),
        VerifyTest.hello(.parse, null),
        VerifyTest.help(.build),
        VerifyTest.hello(.build, null),
        //FIXME: VerifyTest.hello(.build, &[_][]const u8{ "--format=compact" }),
        VerifyTest.help(.run),
        VerifyTest.hello(.run, null),
        VerifyTest.ok(.run, "samples/hello.wat", null),
        VerifyTest.ok(.run, "test/flat/hello.wat", null),
        VerifyTest.ok(.run, "test/hello.wasm", null),
        VerifyTest.ok(.run, "samples/fib.wala", &[_][]const u8{ "--", "--invoke", "fib", "20" }),
        VerifyTest.ok(.run, "test/flat/fib.wat", &[_][]const u8{ "--", "--invoke", "fib", "20" }),
        VerifyTest.errTest(.parse, "fileNotFound.wat", null),
        VerifyTest.errTest(.parse, "topLevelIndent.wala", null),
        VerifyTest.errTest(.parse, "indentMismatch.wala", null),
        VerifyTest.errTest(.build, "typeMismatch.wat", null),
        VerifyTest.ok(.build, "test/custom/importMem.wat", null),
    };

    const verify_step = b.step("verify", "Run behavioral tests");
    verify_step.dependOn(&exe.install_step.?.step);

    const exe_path = b.getInstallPath(.bin, exe.name);
    for (verify_tests) |verif, i| {
        const verify_test_step = std.build.RunStep.create(b, b.fmt("verify {}", .{i}));

        if (coverage) verify_test_step.addArgs(kcov_args);
        verify_test_step.addArg(exe_path);
        verif.apply(verify_test_step);
        verify_step.dependOn(&verify_test_step.step);
    }
    const verify_passed_step = b.addLog("All {} verifications passed.\n", .{verify_tests.len});
    verify_step.dependOn(&verify_passed_step.step);

    const test_step = b.step("test", "Run all tests");
    test_step.dependOn(unit_test_step);
    test_step.dependOn(verify_step);
}

const VerifyTest = struct {
    const Do = enum { parse, build, run };
    do: ?Do = null,
    on: ?[]const u8 = null,
    args: []const []const u8 = &[_][]u8{},
    success: bool = true,

    fn help(do: Do) VerifyTest {
        return ok(do, "--help", null);
    }
    fn hello(do: Do, args: ?[]const []const u8) VerifyTest {
        return ok(do, "samples/hello.wala", args);
    }
    fn ok(do: Do, on: []const u8, args: ?[]const []const u8) VerifyTest {
        return .{ .do = do, .on = on, .args = args orelse &[_][]u8{} };
    }
    fn errTest(do: Do, comptime with: []const u8, args: ?[]const []const u8) VerifyTest {
        return .{ .success = false, .do = do, .on = "samples/error/" ++ with, .args = args orelse &[_][]u8{} };
    }

    fn apply(self: VerifyTest, step: *std.build.RunStep) void {
        //TODO: expected Stdio
        step.stdout_action = .ignore;
        step.stderr_action = .ignore;
        step.expected_exit_code = @boolToInt(!self.success);

        if (self.do) |do| step.addArg(@tagName(do));
        if (self.on) |on| step.addArg(on);
        step.addArgs(self.args);
    }
};
