const std = @import("std");
const Reader = @import("Reader.zig");
const Runner = @import("Runner.zig");
const Wasm = @import("Wasm.zig");
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

    var s = Wasm.init(std.heap.page_allocator);
    defer s.deinit();

    const fd_write_type = Wasm.FuncType{ .params = Wasm.i32Valtypes(4), .returns = Wasm.i32Valtypes(1) };
    const fd_write_idx = try s.addFuncImport("wasi_snapshot_preview1", "fd_write", fd_write_type);

    const _start_type = Wasm.FuncType{ .params = Wasm.i32Valtypes(0), .returns = Wasm.i32Valtypes(0) };
    const _start_idx = try s.addFunc(_start_type, .{});
    try s.addExport("_start", .{ .function = _start_idx });

    _ = s.addDataZeros(8);
    const _start_data = "Hello from WASM-WASI\n";
    const _start_data_offset = try s.addData(_start_data, false);
    std.debug.assert(_start_data_offset == 8);
    try s.exportMemory();

    var c = try s.codeWriter(0, Wasm.emptyValtypes);
    defer c.deinit();
    try c.i32_(40);
    try c.i32_(8);
    try c.i32_store(.{});
    try c.i32_(40);
    try c.i32_(21);
    try c.i32_store(.{ .offset = 4 });
    try c.i32_(1);
    try c.i32_(40);
    try c.i32_(1);
    try c.i32_(0);
    try c.call(fd_write_idx);
    try c.drop();
    s.defineFunc(_start_idx, try s.allocBytes(try c.finish()));

    const stdout = std.io.getStdOut().writer();
    try s.emit(@TypeOf(stdout), stdout);
}
