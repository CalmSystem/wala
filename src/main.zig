const std = @import("std");
const Reader = @import("Reader.zig");
const Runner = @import("Runner.zig");

pub fn main() anyerror!u8 {
    if (std.os.argv.len != 2 and std.os.argv.len != 3) {
        std.log.err("Usage {s} (run) <file>", .{std.os.argv[0]});
        return 1;
    }
    const filePath = std.mem.span(std.os.argv[std.os.argv.len-1]);

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    var reader = try Reader.init(arena.allocator());

    switch (reader.tryParse(filePath)) {
        .ok => |ok| {
            const stdout = std.io.getStdOut().writer();
            try Runner.init(arena.allocator()).runLog(ok, @TypeOf(stdout), stdout);
        },
        .err => |err| {
            std.log.err("{/}{}\n{^}", .{ err.at, err.kind, err.at });
            return 1;
        },
    }
    return 0;
}
