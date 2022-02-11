const std = @import("std");
const Reader = @import("Reader.zig");
const Runner = @import("Runner.zig");
const args = @import("args");

const TopOptions = struct {
    help: bool = false,

    pub const shorthands = .{
        .h = "help",
    };
};
const Command = union(enum) {
    run: EvalOptions,
    build: BuildOptions,
    eval: EvalOptions,
    repl: ReplOptions,
    help: struct {},
    //TODO: fmt,
    //MAYBE: version,
};
const Positionals = [][:0]const u8;

const errPrint = std.debug.print;
fn top_usage(fatal: bool) noreturn {
    errPrint("{s}", .{
        \\Usage: wala <command> [options]
        \\
        \\Commands:
        \\  run     Run file immediatly. Method may vary: eval for now
        \\  build   Compile file to WebAssembly
        \\  eval    Run file with interpreter
        \\  repl    Interactive shell
        \\  help    Print this usage information
        \\
        \\Options:
        \\  -h, --help    Print command-specific usage
        \\
        \\Examples:
        \\  wala run sample/fib.zig
        \\
    });
    std.os.exit(@boolToInt(fatal));
}

pub fn main() !void {
    const argv = args.parseWithVerbForCurrentProcess(TopOptions, Command, std.heap.page_allocator, .print) catch top_usage(true);
    defer argv.deinit();

    const help = argv.options.help;
    const command = argv.verb orelse top_usage(!help);
    switch (command) {
        .build => |options| try build(options, argv.positionals, help),
        .eval, .run => |options| try eval(options, argv.positionals, help),
        .repl => |options| try repl(options, argv.positionals, help),
        .help => top_usage(false),
    }
}

const EvalOptions = struct {
    log: bool = false,
};
fn eval_usage(fatal: bool) noreturn {
    errPrint("{s}", .{
        \\Usage: wala eval <file> [options]
        \\
        \\Options:
        \\  --log         Print result of top level expressions
        \\  -h, --help    Print this usage information
        \\
        \\Examples:
        \\  wala eval sample/fib.zig
        \\
    });
    std.os.exit(@boolToInt(fatal));
}
inline fn eval(options: EvalOptions, positionals: Positionals, help: bool) !void {
    if (help) eval_usage(false);
    if (positionals.len != 1) {
        errPrint("Expect 1 argument got {}\n", .{positionals.len});
        eval_usage(true);
    }

    var reader = try Reader.init(std.heap.page_allocator);
    defer reader.deinit();

    switch (reader.tryParse(positionals[0])) {
        .ok => |ok| {
            var gpa = std.heap.GeneralPurposeAllocator(.{}){};
            defer _ = gpa.deinit();

            var runner = Runner.init(gpa.allocator());
            defer runner.deinit();

            if (options.log) {
                const stdout = std.io.getStdOut().writer();
                try runner.runLog(ok, @TypeOf(stdout), stdout);
            } else {
                try runner.run(ok);
            }
        },
        .err => |err| {
            errPrint("{/}{}\n{^}", .{ err.at, err.kind, err.at });
            std.os.exit(1);
        },
    }
}

const ReplOptions = struct {};
fn repl_usage(fatal: bool) noreturn {
    errPrint("{s}", .{
        \\Usage: wala repl [options]
        \\
        \\Options:
        \\  -h, --help    Print this usage information
        \\
    });
    std.os.exit(@boolToInt(fatal));
}
inline fn repl(_: ReplOptions, positionals: Positionals, help: bool) !void {
    if (help) repl_usage(false);
    if (positionals.len != 0) repl_usage(true);

    @panic("TODO: repl");
}

const BuildOptions = struct {};
fn build_usage(fatal: bool) noreturn {
    errPrint("{s}", .{
        \\Usage: wala build <file> [options]
        \\
        \\Options:
        \\  -h, --help    Print this usage information
        \\
    });
    std.os.exit(@boolToInt(fatal));
}
inline fn build(_: BuildOptions, positionals: Positionals, help: bool) !void {
    if (help) build_usage(false);
    if (positionals.len != 0) build_usage(true);

    @panic("TODO: build");
}
