const std = @import("std");
const u = @import("util.zig");
pub const Expr = @import("Expr.zig");
pub const TextIterator = @import("TextIterator.zig");

pub fn parseAll(iter: *TextIterator, gpa: std.mem.Allocator) Error!Expr.Root {
    var arena = std.heap.ArenaAllocator.init(gpa);
    errdefer arena.deinit();

    var exprs = std.ArrayList(Expr).init(arena.allocator());
    while (!iter.eof()) {
        if (try mayParseOne(iter, arena.allocator())) |expr|
            try exprs.append(expr);
    }
    return .{ .val = .{ .val = .{ .list = exprs.toOwnedSlice() } }, .arena = arena };
}

/// Does not skip block comments
fn skipSpaces(iter: *TextIterator) !void {
    while (true) {
        _ = iter.readWhile(TextIterator.isSpace);
        if (iter.peek().scalar != ';') break;

        iter.skip();
        if (iter.peek().scalar != ';') return error.UnexpectedCharacter;
        iter.skip();

        _ = iter.readWhile(struct {
            fn pred(cp: u21) bool {
                return cp != '\n';
            }
        }.pred);
    }
}
pub const Error = error{
    UnexpectedEndOfFile,
    UnexpectedCharacter,
    OutOfMemory,
    InvalidUtf8,
};
fn mayParseOneBlock(iter: *TextIterator, alloc: std.mem.Allocator, infix: bool) Error!?std.ArrayList(Expr) {
    const open: u21 = if (infix) '{' else '(';
    const close: u21 = if (infix) '}' else ')';

    std.debug.assert(iter.peek().scalar == open);
    iter.skip();

    if (iter.peek().scalar == ';') { // Block comment
        iter.skip();
        while (iter.peek().scalar != close) {
            if (iter.eof())
                return error.UnexpectedEndOfFile;

            _ = iter.readWhile(struct {
                fn pred(cp: u21) bool {
                    return cp != ';';
                }
            }.pred);
            iter.skip();
        }
        iter.skip();

        return null;
    }
    // List
    try skipSpaces(iter);

    var list = std.ArrayList(Expr).init(alloc);
    while (iter.peek().scalar != close) {
        if (iter.eof())
            return error.UnexpectedEndOfFile;

        if (try mayParseOne(iter, alloc)) |expr|
            try list.append(expr);

        try skipSpaces(iter);
    }
    iter.skip();

    if (infix)
        transformInfix(&list);

    return list;
}
inline fn digit(c: u21) !u8 {
    const t = @truncate(u8, c);
    return switch (c) {
        '0'...'9' => t - '0',
        'A'...'F' => t - 'A' + 10,
        'a'...'f' => t - 'a' + 10,
        else => error.UnexpectedCharacter,
    };
}
inline fn nextScalar(iter: *TextIterator) !u21 {
    const cp = iter.next() orelse return error.UnexpectedEndOfFile;
    return cp.scalar;
}
inline fn mayParseOneVal(iter: *TextIterator, alloc: std.mem.Allocator) Error!?Expr.Val {
    const infix = iter.peek().scalar == '{';
    if (iter.peek().scalar == '(' or infix) {
        return if (try mayParseOneBlock(iter, alloc, infix)) |list|
            Expr.Val{ .list = list.items }
        else
            null;
    } else { // String
        const dollared = iter.peek().scalar == '$';
        if (dollared) iter.skip();

        var str = std.ArrayList(u8).init(alloc);
        const quoted = iter.peek().scalar == '"';
        if (quoted) {
            iter.skip();
            while (iter.peek().scalar != '"') {
                if (iter.eof())
                    return error.UnexpectedEndOfFile;

                if (iter.peek().scalar == '\\') {
                    const n = try nextScalar(iter);
                    switch (n) {
                        't' => try str.append('\t'),
                        'n' => try str.append('\n'),
                        'r' => try str.append('\r'),
                        '"' => try str.append('"'),
                        '\'' => try str.append('\''),
                        '\\' => try str.append('\\'),
                        'u' => {
                            if ((try nextScalar(iter)) != '{')
                                return error.UnexpectedCharacter;

                            // hexnum
                            var cp: u21 = try digit(try nextScalar(iter));
                            while (true) {
                                var c = try nextScalar(iter);
                                if (c == '}') break;
                                if (c == '_') c = try nextScalar(iter);
                                cp = std.math.mul(u21, cp, 16) catch return error.InvalidUtf8;
                                cp = std.math.add(u21, cp, try digit(c)) catch return error.InvalidUtf8;
                            }

                            var buf: [4]u8 = undefined;
                            const len = std.unicode.utf8Encode(cp, &buf) catch return error.InvalidUtf8;
                            try str.appendSlice(buf[0..len]);
                        },
                        else => {
                            const m = try nextScalar(iter);
                            try str.append((try digit(n)) * 16 + try digit(m));
                        },
                    }
                } else {
                    try str.appendSlice(iter.peek().bytes);
                }
                iter.skip();
            }
            iter.skip();
        } else {
            const slice = iter.readWhile(struct {
                fn pred(cp: u21) bool {
                    return switch (cp) {
                        ';', '(', ')', '{', '}', '[', ']' => false,
                        else => !TextIterator.isSpace(cp),
                    };
                }
            }.pred);
            if (slice.len == 0) return error.UnexpectedCharacter;
            try str.appendSlice(slice);
        }

        const text = str.toOwnedSlice();
        if (dollared) {
            return Expr.Val{ .id = try u.toTxt(text) };
        } else if (quoted) {
            return Expr.Val{ .string = text };
        } else return Expr.Val{ .keyword = text };
    }
}

pub fn mayParseOne(iter: *TextIterator, arena: std.mem.Allocator) Error!?Expr {
    try skipSpaces(iter);
    const at_offset = iter.peek().offset;

    const left = if (try mayParseOneVal(iter, arena)) |val|
        Expr{ .at = .{ .offset = at_offset, .len = iter.peek().offset - at_offset }, .val = val }
    else
        return null;

    // Partial neoteric-expression
    if (iter.peek().scalar == '{') {
        const list = try arena.alloc(Expr, 2);
        list[0] = left;
        list[1] = (try mayParseOne(iter, arena)).?; // Always infix list

        return Expr{ .at = .{ .offset = at_offset, .len = iter.peek().offset - at_offset }, .val = .{ .list = list } };
    } else if (iter.peek().scalar == '(') {
        var exprs = (try mayParseOneBlock(iter, arena, false)) orelse return left;
        try exprs.insert(0, left);

        return Expr{ .at = .{ .offset = at_offset, .len = iter.peek().offset - at_offset }, .val = .{ .list = exprs.toOwnedSlice() } };
    }
    return left;
}

inline fn transformInfix(list: *std.ArrayList(Expr)) void {
    const es = list.items;
    if (es.len < 2) return;

    std.mem.swap(Expr, &es[0], &es[1]);

    if (es.len < 5 or es.len % 2 != 1) return;

    const op = es[0];
    var i: usize = 3;
    while (i < es.len) : (i += 2) {
        if (!op.val.shallowEql(es[i].val)) return;
    }

    i = 3;
    while (i < list.items.len - 1) : (i += 1) {
        _ = list.orderedRemove(i);
    }
}
