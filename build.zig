const std = @import("std");
const deps = @import("deps.zig");

pub fn build(b: *std.build.Builder) void {
    const target = b.standardTargetOptions(.{});
    const mode = b.standardReleaseOptions();

    const exe = b.addExecutable("wala", "src/main.zig");
    exe.setTarget(target);
    exe.setBuildMode(mode);
    deps.addAllTo(exe);
    exe.install();

    const run_cmd = exe.run();
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const exe_tests = b.addTest("src/main.zig");
    exe_tests.setTarget(target);
    exe_tests.setBuildMode(mode);
    deps.addAllTo(exe_tests);
    const coverage = b.option(bool, "coverage", "Generate coverage with kcov on test") orelse false;
    if (coverage) {
        exe_tests.setExecCmd(&[_]?[]const u8{
            "kcov",
            "--include-path=src",
            b.getInstallPath(.prefix, "coverage"),
            null, // to get zig to use the --test-cmd-bin flag
        });
    }
    const test_step = b.step("test", "Run all tests");
    test_step.dependOn(&exe_tests.step);

    const fmt = b.addFmt(&[_][]const u8{ "src", "build.zig" });
    const fmt_step = b.step("fmt", "Format all code");
    fmt_step.dependOn(&fmt.step);
}
