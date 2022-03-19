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
