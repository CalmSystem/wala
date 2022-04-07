const std = @import("std");
const SParser = @import("SParser.zig");
const Expr = SParser.Expr;
const TextIterator = SParser.TextIterator;

pub const Error = error{
    TopLevelIndent,
    IndentMismatch,
} || SParser.Error;

pub fn parseAll(iter: *TextIterator, alloc: std.mem.Allocator) ![]Expr {
    var exprs = std.ArrayList(Expr).init(alloc);
    while (!iter.eof()) {
        if (try mayParseTopOne(iter, alloc)) |expr|
            try exprs.append(expr);
    }
    return exprs.toOwnedSlice();
}

fn notEol(cp: u21) bool {
    return cp != '\n';
}
fn isPadding(cp: u21) bool {
    return notEol(cp) and TextIterator.isSpace(cp);
}
inline fn isIndented(str: []const u8) bool {
    return str.len > 0 and (str.len != 1 or str[0] != '\r');
}

fn readPadding(iter: *TextIterator) ?[]const u8 {
    const indent = iter.readWhile(isPadding);

    if (iter.peek().scalar != ';')
        return indent;

    // Full line comment
    iter.skip();
    std.debug.assert(iter.peek().scalar == ';');
    iter.skip();
    _ = iter.readWhile(notEol);
    return null;
}
fn parseIndent(iter: *TextIterator) []const u8 {
    while (true) {
        if (readPadding(iter)) |indent| {
            if (!iter.eol()) return indent;
        }
        iter.skip();
    }
}

pub fn mayParseTopOne(iter: *TextIterator, alloc: std.mem.Allocator) !?Expr {
    if (isIndented(parseIndent(iter)))
        return error.TopLevelIndent;

    if (try mayParseOne(iter, alloc, "")) |ei| {
        if (ei.next_indent.len != 0)
            return error.IndentMismatch;
        return ei.expr;
    }
    return null;
}

const ExprIndent = struct { expr: Expr, next_indent: []const u8 };
fn mayParseOne(iter: *TextIterator, alloc: std.mem.Allocator, my_indent: []const u8) SParser.Error!?ExprIndent {
    if (iter.eof()) return null;

    //TODO: special blocks like \\
    const left = (try SParser.mayParseOne(iter, alloc)) orelse return null;

    var exprs = std.ArrayList(Expr).init(alloc);
    while (true) {
        _ = readPadding(iter);
        if (iter.eof() or iter.eol()) break;

        if (try SParser.mayParseOne(iter, alloc)) |next| {
            if (exprs.items.len == 0)
                try exprs.append(left);
            try exprs.append(next);
        }
    }
    iter.skip();

    var next_indent = parseIndent(iter);
    if (next_indent.len > my_indent.len and std.mem.startsWith(u8, next_indent, my_indent)) {
        const child_indent = next_indent;
        while (std.mem.eql(u8, next_indent, child_indent)) {
            if (try mayParseOne(iter, alloc, child_indent)) |ei| {
                if (exprs.items.len == 0)
                    try exprs.append(left);
                try exprs.append(ei.expr);
                next_indent = ei.next_indent;
            }
        }
    }

    return ExprIndent{ .expr = if (exprs.items.len == 0) left else Expr{ .at = .{ .offset = left.at.?.offset, .len = exprs.items[exprs.items.len - 1].at.?.end() - left.at.?.offset }, .val = .{ .list = exprs.toOwnedSlice() } }, .next_indent = next_indent };
}

inline fn deinitAll(arr: []Expr, allocator: std.mem.Allocator) void {
    for (arr) |expr|
        expr.deinit(allocator);
    allocator.destroy(arr.ptr);
}
/// https://github.com/ziglang/zig/issues/4437
inline fn expectEqual(expected: anytype, actual: anytype) !void {
    return std.testing.expectEqual(@as(@TypeOf(actual), expected), actual);
}
test "empty" {
    var iter = TextIterator.unsafeInit("");
    const empty = try parseAll(&iter, std.testing.failing_allocator);

    try expectEqual(0, empty.len);
}
test "skip spaces" {
    var iter = try TextIterator.init("\n() (;b ;) \r\n;; comment\n(  )");
    const two = try parseAll(&iter, std.testing.allocator);
    defer deinitAll(two, std.testing.allocator);

    try expectEqual(2, two.len);
    try std.testing.expect(two[1].val == .list);
    try expectEqual(0, two[1].val.list.len);
    try expectEqual(24, two[1].at.?.offset);
    try expectEqual(4, two[1].at.?.len);
}
test "strings" {
    var iter = try TextIterator.init(
        \\"a\n\t\r\\b\'\"c\64\u{65}"
        \\$id
        \\$"still id"
    );
    const str = try parseAll(&iter, std.testing.allocator);
    defer deinitAll(str, std.testing.allocator);

    try expectEqual(3, str.len);
    try std.testing.expectEqualStrings("a\n\t\r\\b\'\"cde", str[0].val.string);
    try std.testing.expectEqualStrings("id", str[1].val.id);
    try std.testing.expectEqualStrings("still id", str[2].val.id);
}

test "infix" {
    var iter = try TextIterator.init(
        \\{}
        \\{a}
        \\{b a c}
        \\{a + b + c}
    );
    const infix = try parseAll(&iter, std.testing.allocator);
    defer deinitAll(infix, std.testing.allocator);

    try expectEqual(4, infix.len);
    for (infix) |list| {
        try std.testing.expect(list.val == .list);
        for (list.val.list) |kv|
            try std.testing.expect(kv.val == .keyword);
    }

    try expectEqual(0, infix[0].val.list.len);
    try expectEqual(1, infix[1].val.list.len);
    try expectEqual(3, infix[2].val.list.len);
    try std.testing.expectEqualStrings("a", infix[2].val.list[0].val.keyword);
    try std.testing.expectEqualStrings("b", infix[2].val.list[1].val.keyword);
    try std.testing.expectEqualStrings("c", infix[2].val.list[2].val.keyword);
    try expectEqual(4, infix[3].val.list.len);
    try std.testing.expectEqualStrings("+", infix[3].val.list[0].val.keyword);
    try std.testing.expectEqualStrings("a", infix[3].val.list[1].val.keyword);
    try std.testing.expectEqualStrings("b", infix[3].val.list[2].val.keyword);
    try std.testing.expectEqualStrings("c", infix[3].val.list[3].val.keyword);
}
test "neoteric" {
    var iter = try TextIterator.init(
        \\a (b)
        \\a(b)
        \\a{c b}
    );
    const neoteric = try parseAll(&iter, std.testing.allocator);
    defer deinitAll(neoteric, std.testing.allocator);

    try expectEqual(3, neoteric.len);
    for (neoteric) |list| {
        try std.testing.expect(list.val == .list);
        try expectEqual(2, list.val.list.len);
        try std.testing.expectEqualStrings("a", list.val.list[0].val.keyword);
    }

    try expectEqual(1, neoteric[0].val.list[1].val.list.len);
    try std.testing.expectEqualStrings("b", neoteric[1].val.list[1].val.keyword);
    try expectEqual(2, neoteric[2].val.list[1].val.list.len);
    try std.testing.expectEqualStrings("b", neoteric[2].val.list[1].val.list[0].val.keyword);
}
test "Expr.format" {
    var iter = TextIterator.unsafeInit("a ($b) \"c\" d");
    const exprs = try parseAll(&iter, std.testing.allocator);
    defer deinitAll(exprs, std.testing.allocator);

    try expectEqual(1, exprs.len);
    try std.testing.expectFmt(
        \\(a ($b) "c" d)
    , "{}", .{exprs[0]});
    try std.testing.expectFmt(
        \\(a
        \\  ($b)
        \\  "c"
        \\  d)
    , "{tree}", .{exprs[0]});
    try std.testing.expectFmt(
        \\a
        \\  $b
        \\  "c" d
    , "{sweet}", .{exprs[0]});
}

test "Error.TopLevelIndent" {
    var iter = TextIterator.unsafeInit("   42");
    const err = parseAll(&iter, std.testing.failing_allocator);

    try std.testing.expectError(Error.TopLevelIndent, err);
    try expectEqual(3, iter.cur.?.offset);
}
test "Error.IndentMismatch" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    var iter = TextIterator.unsafeInit(
        \\top
        \\  (2;;
        \\)
        \\ 1
    );
    const err = parseAll(&iter, arena.allocator());

    try std.testing.expectError(Error.IndentMismatch, err);
    try expectEqual(14, iter.cur.?.offset);
}
test "Error.InvalidUtf8 Surrogate" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    var iter = TextIterator.unsafeInit(
        \\"\u{d842}"
    );
    const err = parseAll(&iter, arena.allocator());

    try std.testing.expectError(Error.InvalidUtf8, err);
    try expectEqual(8, iter.cur.?.offset);
}
test "Error.InvalidUtf8 Overflow" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    var iter = TextIterator.unsafeInit(
        \\"\u{AAAAAAAAAAAAAAAAAAAAAAAAA}"
    );
    const err = parseAll(&iter, arena.allocator());

    try std.testing.expectError(Error.InvalidUtf8, err);
    try expectEqual(9, iter.cur.?.offset);
}
test "Error.UnexpectedCharacter" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    var iter = TextIterator.unsafeInit(
        \\"\error"
    );
    const err = parseAll(&iter, arena.allocator());

    try std.testing.expectError(Error.UnexpectedCharacter, err);
    try expectEqual(3, iter.cur.?.offset);
}
