const std = @import("std");
pub const Expr = @import("Expr.zig");
pub const TextIterator = @import("TextIterator.zig");

pub fn parseAll(iter: *TextIterator, alloc: std.mem.Allocator) Error![]Expr {
    var exprs = std.ArrayList(Expr).init(alloc);
    while (!iter.eof()) {
        if (try mayParseOne(iter, alloc)) |expr|
            try exprs.append(expr);
    }
    return exprs.toOwnedSlice();
}

/// Does not skip block comments
fn skipSpaces(iter: *TextIterator) void {
    while (true) {
        _ = iter.readWhile(TextIterator.isSpace);
        if (iter.peek().scalar != ';') break;

        iter.skip();
        std.debug.assert(iter.peek().scalar == ';');
        iter.skip();
        
        _ = iter.readWhile(struct {
            fn pred(cp: u21) bool { return cp != '\n'; }
        }.pred);
    }
}
pub const Error = error {
    UnexpectedEndOfFile,
    OutOfMemory,
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
    skipSpaces(iter);

    var list = std.ArrayList(Expr).init(alloc);
    while (iter.peek().scalar != close) {
        if (iter.eof())
            return error.UnexpectedEndOfFile;

        if (try mayParseOne(iter, alloc)) |expr|
            try list.append(expr);

        skipSpaces(iter);
    }
    iter.skip();

    if (infix)
        transformInfix(&list);

    return list;
}
inline fn mayParseOneVal(iter: *TextIterator, alloc: std.mem.Allocator) Error!?Expr.Val {
    const infix = iter.peek().scalar == '{';
    if (iter.peek().scalar == '(' or infix) {
        return if (try mayParseOneBlock(iter, alloc, infix)) |list|
            Expr.Val{ .list = list.items } else null;

    } else { // String
        const dollared = iter.peek().scalar == '$';
        if (dollared) iter.skip();

        var text: []const u8 = "";
        const quoted = iter.peek().scalar == '"';
        if (quoted) {
            iter.skip();
            var str = std.ArrayList(u8).init(alloc);
            while (iter.peek().scalar != '"') {
                if (iter.eof())
                    return error.UnexpectedEndOfFile;

                if (iter.peek().scalar == '\\') {
                    //TODO: unescape string
                }
                try str.appendSlice(iter.peek().bytes);
                iter.skip();
            }
            iter.skip();

            text = str.toOwnedSlice();
        } else {
            text = iter.readWhile(struct {
                fn pred(cp: u21) bool {
                    return switch(cp) {
                        ';','(',')','{','}','[',']' => false,
                        else => !TextIterator.isSpace(cp)
                    };
                }
            }.pred);
        }

        if (dollared) {
            return Expr.Val{ .ident = text };
        } else if (quoted) {
            return Expr.Val{ .str = text };
        } else
            return Expr.Val{ .name = text };
    }
}

pub fn mayParseOne(iter: *TextIterator, alloc: std.mem.Allocator) Error!?Expr {
    skipSpaces(iter);
    const at_offset = iter.peek().offset;

    const left = if (try mayParseOneVal(iter, alloc)) |val|
        Expr{
            .at = .{ .offset = at_offset, .len = iter.peek().offset-at_offset },
            .val = val
        } else return null;
    
    // Partial neoteric-expression
    if (iter.peek().scalar == '{') {
        const list = try alloc.alloc(Expr, 2);
        list[0] = left;
        list[1] = (try mayParseOne(iter, alloc)).?; // Always infix list

        return Expr{
            .at = .{ .offset = at_offset, .len = iter.peek().offset-at_offset },
            .val = .{ .list = list }
        };
    } else if (iter.peek().scalar == '(') {
        var exprs = (try mayParseOneBlock(iter, alloc, false)) orelse return left;
        try exprs.insert(0, left);

        return Expr{
            .at = .{ .offset = at_offset, .len = iter.peek().offset-at_offset },
            .val = .{ .list = exprs.toOwnedSlice() }
        };
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
    while (i < es.len): (i += 2) {
        if (!op.val.shallowEql(es[i].val)) return;
    }

    i = 3;
    while (i < list.items.len-1): (i += 1) {
        _ = list.orderedRemove(i);
    }
}

