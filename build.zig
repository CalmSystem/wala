const std = @import("std");
const deps = @import("deps.zig");

pub fn build(b: *std.build.Builder) void {
    const target = b.standardTargetOptions(.{});
    const mode = b.standardReleaseOptions();

    const exe = b.addExecutable("wala", "src/main.zig");
    const exe_tests = b.addTest("src/main.zig");
    const exe_cases = b.addExecutable("cases", "test/cases.zig");

    for (&[_]*std.build.LibExeObjStep{ exe, exe_tests }) |e| {
        e.setTarget(target);
        e.setBuildMode(mode);
        e.single_threaded = true;
        deps.addAllTo(e);
    }

    exe.install();

    const fmt = b.addFmt(&[_][]const u8{ "src", "test", "build.zig" });
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

    const cases_run = exe_cases.run();
    if (coverage) cases_run.addArgs(kcov_args);
    cases_run.addArg(b.getInstallPath(.bin, exe.name));
    cases_run.step.dependOn(&exe.install_step.?.step);
    const cases_step = b.step("cases-test", "Run behavioral tests");
    cases_step.dependOn(&cases_run.step);

    const test_step = b.step("test", "Run all tests");
    test_step.dependOn(unit_test_step);
    test_step.dependOn(cases_step);
}
