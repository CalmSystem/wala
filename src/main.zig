const std = @import("std");
const args = @import("args");
const File = @import("File.zig");
const Loader = @import("Loader.zig");

const TopOptions = struct {
    help: bool = false,

    pub const shorthands = .{
        .h = "help",
    };
};
const Command = union(enum) {
    run: RunOptions,
    build: BuildOptions,
    parse: ParseOptions,
    help: struct {},
};
const Positionals = [][:0]const u8;

const errPrint = std.debug.print;
fn top_usage(fatal: bool) noreturn {
    errPrint("{s}", .{
        \\Usage: wala <command> [options]
        \\
        \\Commands:
        \\  run     Run built wasm with system runtime
        \\  build   Convert Wala to WebAssembly
        \\  parse   Read sweet expressions
        \\  help    Print this usage information
        \\
        \\Options:
        \\  -h, --help    Print command-specific usage
        \\
        \\Examples:
        \\  wala run samples/hello.wala
        \\
    });
    std.os.exit(@boolToInt(fatal));
}
fn fatalErr(err: anytype) noreturn {
    errPrint("fatal: {}\n", .{err});
    std.os.exit(1);
}

const top_alloc = std.heap.page_allocator;

pub fn main() void {
    const argv = args.parseWithVerbForCurrentProcess(TopOptions, Command, top_alloc, .print) catch top_usage(true);
    defer argv.deinit();

    const help = argv.options.help;
    const command = argv.verb orelse top_usage(!help);

    switch (command) {
        .run => |options| run(options, argv.positionals, help),
        .build => |options| build(options, argv.positionals, help),
        .parse => |options| parse(options, argv.positionals, help),
        .help => top_usage(false),
    }
}

const ParseOptions = struct {
    format: @import("Expr.zig").Format = .human,
};
fn parse_usage(fatal: bool) noreturn {
    errPrint("{s}", .{
        \\Usage: wala parse <file> [options]
        \\
        \\Options:
        \\  --format      Output format (compact, tree, human, sweet)
        \\  -h, --help    Print this usage information
        \\
        \\Examples:
        \\  wala parse samples/hello.wala
        \\
    });
    std.os.exit(@boolToInt(fatal));
}
inline fn parse(options: ParseOptions, positionals: Positionals, help: bool) void {
    if (help) parse_usage(false);
    if (positionals.len != 1) {
        errPrint("Expect 1 argument got {}\n", .{positionals.len});
        parse_usage(true);
    }

    var file = File.read(positionals[0], top_alloc) catch |err| fatalErr(err);
    defer file.deinit();

    switch (file.text.tryRead()) {
        .ok => |exprs| {
            const writer = std.io.getStdOut().writer();
            for (exprs) |expr| {
                expr.print(options.format, writer) catch unreachable;
                writer.writeByte('\n') catch unreachable;
            }
        },
        .err => |err| fatalErr(err),
    }
}

inline fn aLoader() Loader {
    return .{
        .allocator = top_alloc,
        .errAt = struct {
            fn do(err: Loader.ErrPoint, data: Loader.ErrData) void {
                errPrint("{}\n", .{err});
                if (data) |d| errPrint("{}\n", .{d});
            }
        }.do,
    };
}

const BuildOptions = struct {
    format: enum { compact, human, binary } = .binary,
};
fn build_usage(fatal: bool) noreturn {
    errPrint("{s}", .{
        \\Usage: wala build <file> [options]
        \\
        \\Options:
        \\  --format      Output format (compact, human, binary)
        \\  -h, --help    Print this usage information
        \\
        \\Examples:
        \\  wala build samples/hello.wala
        \\
    });
    std.os.exit(@boolToInt(fatal));
}
inline fn build(options: BuildOptions, positionals: Positionals, help: bool) void {
    if (help) build_usage(false);
    if (positionals.len != 1) {
        errPrint("Expect 1 argument got {}\n", .{positionals.len});
        build_usage(true);
    }

    var loader = aLoader();
    const module = loader.load(positionals[0]) catch |err| fatalErr(err);
    defer module.deinit();

    const writer = std.io.getStdOut().writer();
    switch (options.format) {
        .binary => Loader.writeWasm(module, writer) catch unreachable,
        .compact => Loader.writeText(module, writer, loader.allocator, .compact) catch unreachable,
        .human => Loader.writeText(module, writer, loader.allocator, .human) catch unreachable,
    }
}

const RunOptions = struct {
    runtime: []const u8 = "wasmtime",
};
fn run_usage(fatal: bool) noreturn {
    errPrint("{s}", .{
        \\Usage: wala run <file> [options]
        \\
        \\Options:
        \\  --runtime     Execution environment with arguments
        \\  -h, --help    Print this usage information
        \\
        \\Examples:
        \\  wala run samples/hello.wala
        \\
    });
    std.os.exit(@boolToInt(fatal));
}
inline fn run(options: RunOptions, positionals: Positionals, help: bool) void {
    if (help) run_usage(false);
    if (positionals.len == 0) {
        errPrint("Expect at least 1 argument\n", .{});
        run_usage(true);
    }

    var loader = aLoader();
    const module = loader.load(positionals[0]) catch |err| fatalErr(err);
    defer module.deinit();

    var tmpDir = std.testing.tmpDir(.{});
    defer tmpDir.cleanup();

    const wasmName = "run.wasm";

    const wasmFile = tmpDir.dir.createFile(wasmName, .{}) catch unreachable;
    Loader.writeWasm(module, wasmFile.writer()) catch unreachable;
    wasmFile.close();

    const argv = top_alloc.alloc([]const u8, positionals.len + 1) catch unreachable;
    defer top_alloc.free(argv);

    argv[0] = options.runtime;
    argv[1] = wasmName;
    std.mem.copy([]const u8, argv[2..], positionals[1..]);

    const runtime = std.ChildProcess.init(argv, top_alloc) catch unreachable;
    defer runtime.deinit();
    runtime.cwd_dir = tmpDir.dir;
    _ = runtime.spawnAndWait() catch unreachable;
}

test {
    _ = @import("SweetParser.zig");
    _ = @import("Wasm.zig");
}
