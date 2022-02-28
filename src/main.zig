const std = @import("std");
const args = @import("args");
const Expr = @import("Expr.zig");
const File = @import("File.zig");
const Module = @import("Module.zig");

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
    errPrint("{}\n", .{ err });
    std.os.exit(1);
}

const top_alloc = std.heap.page_allocator;


pub fn main() !void {
    const argv = args.parseWithVerbForCurrentProcess(TopOptions, Command, top_alloc, .print) catch top_usage(true);
    defer argv.deinit();

    const help = argv.options.help;
    const command = argv.verb orelse top_usage(!help);

    switch (command) {
        .run => |options| try run(options, argv.positionals, help),
        .build => |options| try build(options, argv.positionals, help),
        .parse => |options| try parse(options, argv.positionals, help),
        .help => top_usage(false),
    }
}

const ParseOptions = struct {
    format: Expr.Format = .human,
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
inline fn parse(options: ParseOptions, positionals: Positionals, help: bool) !void {
    if (help) parse_usage(false);
    if (positionals.len != 1) {
        errPrint("Expect 1 argument got {}\n", .{positionals.len});
        parse_usage(true);
    }

    var file = File.init(positionals[0], top_alloc) catch |err| fatalErr(err);
    switch (file.tryRead()) {
        .ok => |exprs| {
            const writer = std.io.getStdOut().writer();
            for (exprs) |expr| {
                expr.print(options.format, writer) catch unreachable;
                writer.writeByte('\n') catch unreachable;
            }
        },
        .err => |err| fatalErr(err)
    }
}

const BuildFormat = enum { compact, human }; //TODO: binary
const BuildOptions = struct {
    format: BuildFormat = .compact,
};
fn build_usage(fatal: bool) noreturn {
    errPrint("{s}", .{
        \\Usage: wala build <file> [options]
        \\
        \\Options:
        \\  --format      Output format (compact, human)
        \\  -h, --help    Print this usage information
        \\
        \\Examples:
        \\  wala build samples/hello.wala
        \\
    });
    std.os.exit(@boolToInt(fatal));
}
inline fn build(options: BuildOptions, positionals: Positionals, help: bool) !void {
    if (help) build_usage(false);
    if (positionals.len != 1) {
        errPrint("Expect 1 argument got {}\n", .{positionals.len});
        build_usage(true);
    }

    var file = File.init(positionals[0], top_alloc) catch |err| fatalErr(err);
    switch (file.tryRead()) {
        .ok => |exprs| {
            const module = try Module.compile(exprs, file.arena.allocator());

            const writer = std.io.getStdOut().writer();
            switch (options.format) {
                .compact => writer.print("{compact}\n", .{ module }) catch unreachable,
                .human => writer.print("{human}\n", .{ module }) catch unreachable,
            }
        },
        .err => |err| fatalErr(err)
    }
}

const RunOptions = struct {
    runtime: []const u8 = "wasmtime",
    builder: []const u8 = "wat2wasm", // TODO: replace with BuildFormat.binary
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
inline fn run(options: RunOptions, positionals: Positionals, help: bool) !void {
    if (help) run_usage(false);
    if (positionals.len != 1) {
        errPrint("Expect 1 argument got {}\n", .{positionals.len});
        run_usage(true);
    }

    var file = File.init(positionals[0], top_alloc) catch |err| fatalErr(err);
    switch (file.tryRead()) {
        .ok => |exprs| {
            const module = try Module.compile(exprs, file.arena.allocator());

            var tmpDir = std.testing.tmpDir(.{});
            defer tmpDir.cleanup();

            const watName = "run.wat";
            const wasmName = "run.wasm";

            const watFile = tmpDir.dir.createFile(watName, .{}) catch unreachable;
            module.print(.compact, watFile.writer()) catch unreachable;
            watFile.close();
            
            // TODO: check Term
            _ = try std.ChildProcess.exec(.{
                .allocator = top_alloc,
                .argv = &[_][]const u8 {options.builder, watName, "-o", wasmName},
                .cwd_dir = tmpDir.dir
            });

            const runtime = std.ChildProcess.init(&[_][]const u8 {options.runtime, wasmName}, top_alloc) catch unreachable;
            defer runtime.deinit();
            runtime.cwd_dir = tmpDir.dir;
            _ = try runtime.spawnAndWait();
        },
        .err => |err| fatalErr(err)
    }
}
