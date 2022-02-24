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
        \\  build   Compile Wala to Wat
        \\  parse   Read sweet expressions
        \\  help    Print this usage information
        \\
        \\Options:
        \\  -h, --help    Print command-specific usage
        \\
        \\Examples:
        \\  wala build samples/hello.wala
        \\
    });
    std.os.exit(@boolToInt(fatal));
}
fn fatalErr(err: anyerror) noreturn {
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
        .err => |err| {
            err.print(file);
            std.os.exit(1);
        }
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
        .err => |err| {
            err.print(file);
            std.os.exit(1);
        }
    }
}
