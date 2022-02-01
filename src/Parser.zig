const std = @import("std");
const ziglyph = @import("ziglyph");

pub const Name = []const u8;
pub const Node = struct {
    at: Range,
    value: Value,

    pub const Range = struct {
        offset: usize,
        len: usize,

        pub fn view(r: Range, str: []const u8) []const u8 {
            return str[r.offset..r.end()];
        }
        pub fn end(r: Range) usize {
            return r.offset + r.len;
        }
    };

    pub const Value = union(enum) {
        name: Name,
        /// Interger litteral
        integer: i128,
        /// Floating point litteral
        decimal: f128,
        /// Method call
        call: Call,
        /// List of value
        tuple: []Node,
        /// List of expression
        block: Block,

        pub const Call = struct {
            on: *Node,
            method: Name,
            arg: *Node,
        };

        fn createCall(on: Node, method: Name, arg: Node, alloc: std.mem.Allocator) !Value {
            const pOn = try alloc.create(Node);
            const pArg = try alloc.create(Node);
            pOn.* = on;
            pArg.* = arg;

            return Value{ .call = Call{ .on = pOn, .method = method, .arg = pArg } };
        }
        fn isBlock(v: Value) bool {
            return switch (v) {
                Value.block => true,
                else => false,
            };
        }

        const emptyTuple = Value{ .tuple = &[_]Node{} };
    };
};
pub const Block = []Node;

const TextIterator = struct {
    cp_iter: ziglyph.CodePointIterator,
    cur: ?ziglyph.CodePoint,

    const Self = @This();
    const eofBytes = &[_]u8{};

    pub fn init(bytes: []const u8) Self {
        var iter = ziglyph.CodePointIterator{ .bytes = bytes };
        return Self{ .cur = iter.next(), .cp_iter = iter };
    }

    pub fn next(self: *Self) ?ziglyph.CodePoint {
        self.cur = self.cp_iter.next();
        return self.cur;
    }
    pub fn skip(self: *Self) void {
        _ = self.next();
    }
    pub fn peek(self: Self) ziglyph.CodePoint {
        return if (self.cur) |c| c else ziglyph.CodePoint{ .scalar = 0, .offset = self.cp_iter.bytes.len, .bytes = eofBytes };
    }

    pub fn eol(self: Self) bool {
        return self.cur != null and self.cur.?.scalar == '\n';
    }
    pub fn eof(self: Self) bool {
        return self.cur == null;
    }

    pub fn readWhile(self: *Self, comptime pred: fn (u21) bool) []const u8 {
        if (self.cur) |p| {
            while (self.cur != null and pred(self.cur.?.scalar)) {
                self.skip();
            }
            return if (self.cur) |q|
                self.cp_iter.bytes[p.offset..q.offset]
            else
                self.cp_iter.bytes[p.offset..];
        } else return eofBytes;
    }
};

const TUPLE_OPEN = '(';
const TUPLE_CLOSE = ')';
const TUPLE_NEXT = ',';
const DOT = '.';
const LF = '\n';
const DUBL = ':';

inline fn IsToken(cp: u21) bool {
    return switch (cp) {
        0 => true,
        TUPLE_OPEN => true,
        TUPLE_CLOSE => true,
        TUPLE_NEXT => true,
        DOT => true,
        LF => true,
        else => false,
    };
}
inline fn IsEndOfExpr(cp: u21) bool {
    return switch (cp) {
        0 => true,
        TUPLE_CLOSE => true,
        TUPLE_NEXT => true,
        LF => true,
        else => false,
    };
}
fn IsNameCp(cp: u21) bool {
    return !IsToken(cp) and !IsSpace(cp);
}
fn IsSpace(cp: u21) bool {
    return cp != '\n' and ziglyph.isWhiteSpace(cp);
}
inline fn IsIndented(str: []const u8) bool {
    return str.len > 0 and (str.len != 1 or str[0] != '\r');
}

const Error = error{
    /// Top level block cannot start with indent
    TopLevelIndent,
    /// Non top level block must start with indent
    BlockNotIndent,
    /// Expect a name first character
    ExpectName,
    /// Not a digit character
    InvalidDigit,
    /// Digit invalid in choosen base
    DigitExceedsRadix,
    /// 128 bit math overflow
    NumericOverflow,
    /// No enought memory to allocate Nodes
    OutOfMemory,
    /// Tuple openning and closing braces missmatch
    UnclosedTuple,
};
pub const Result = union(enum) {
    ok: Block,
    err: Err,

    const Err = struct {
        kind: Error,
        offset: usize,
    };
};

iter: TextIterator,
allocator: std.mem.Allocator,
indentPending: []const u8 = "",

const Ctx = @This();

pub fn parse(text: []const u8, allocator: std.mem.Allocator) !Block {
    var ctx = Ctx{
        .iter = TextIterator.init(text),
        .allocator = allocator,
    };
    return ctx.parseTop();
}
pub fn tryParse(text: []const u8, allocator: std.mem.Allocator) Result {
    var ctx = Ctx{
        .iter = TextIterator.init(text),
        .allocator = allocator,
    };
    return if (ctx.parseTop()) |ok|
        Result{ .ok = ok }
    else |err| {
        if (@errorReturnTrace()) |trace| {
            std.debug.dumpStackTrace(trace.*);
        }
        return Result{ .err = Result.Err{ .kind = err, .offset = ctx.offset() } };
    };
}

fn parseTop(ctx: *Ctx) Error!Block {
    var arr = std.ArrayList(Node).init(ctx.allocator);
    while (true) {
        if (IsIndented(ctx.indentPending))
            return Error.TopLevelIndent;
        while (ctx.iter.eol()) {
            ctx.iter.skip();
            if (IsIndented(ctx.iter.readWhile(IsSpace)))
                return Error.TopLevelIndent;
        }
        if (ctx.iter.eof())
            break;

        try arr.append(try ctx.parseExpr());
    }
    return arr.items;
}

fn parseBlock(ctx: *Ctx) !Node {
    std.debug.assert(ctx.iter.eol());
    ctx.iter.skip();

    const start = ctx.offset();
    var arr = std.ArrayList(Node).init(ctx.allocator);

    const indent = ctx.iter.readWhile(IsSpace);
    if (!IsIndented(indent))
        return Error.BlockNotIndent;
    //MAYBE: warn if indent not startsWith ctx.indent

    try arr.append(try ctx.parseExpr());
    while (true) {
        while (ctx.iter.eol() and !IsIndented(ctx.indentPending)) {
            ctx.iter.skip();
            ctx.indentPending = ctx.iter.readWhile(IsSpace);
        }
        if (!std.mem.eql(u8, indent, ctx.indentPending))
            break;

        ctx.indentPending = "";
        try arr.append(try ctx.parseExpr());
    }

    return Node{ .at = Node.Range{ .offset = start, .len = arr.items[arr.items.len - 1].at.end() - start }, .value = Node.Value{ .block = arr.items } };
}

fn parseExpr(ctx: *Ctx) Error!Node {
    std.debug.assert(ctx.scalar() != LF);
    var top = try ctx.parseValue();

    while (!IsEndOfExpr(ctx.scalar())) {
        var call = try ctx.readCall();
        const dubl = call.len > 1 and call[call.len-1] == DUBL;
        if (dubl) call = call[0..call.len-1];

        const arg = try if (dubl) ctx.parseExpr() else ctx.parseValue();

        top.at.len = arg.at.end() - top.at.offset;
        top.value = try Node.Value.createCall(top, call, arg, ctx.allocator);
        if (dubl or arg.value.isBlock()) break;
    }
    return top;
}
inline fn readCall(ctx: *Ctx) !Name {
    if (ctx.scalar() == DOT) {
        defer ctx.skipPad();
        return ctx.iter.cur.?.bytes;
    }
    return try ctx.readName();
}

fn parseValue(ctx: *Ctx) Error!Node {
    const cp = ctx.scalar();
    return if (cp == TUPLE_OPEN)
        ctx.parseTuple()
    else if (cp == DOT)
        ctx.parseDot()
    else if (cp == LF)
        ctx.parseBlock()
    else if (ziglyph.isAsciiDigit(cp) or cp == '-')
        ctx.parseNumber()
    else
        ctx.parseName();
}
fn parseDot(ctx: *Ctx) Node {
    std.debug.assert(ctx.scalar() == DOT);
    const start = ctx.offset();
    ctx.iter.skip();
    defer ctx.padding();
    return Node{ .at = ctx.range(start), .value = Node.Value.emptyTuple };
}
fn parseTuple(ctx: *Ctx) !Node {
    std.debug.assert(ctx.scalar() == TUPLE_OPEN);
    const start = ctx.offset();
    ctx.skipPad();

    var arr = std.ArrayList(Node).init(ctx.allocator);
    while (ctx.scalar() != TUPLE_CLOSE) {
        try arr.append(try ctx.parseExpr());
        if (ctx.scalar() != TUPLE_NEXT)
            break;
        ctx.skipPad();
    }

    if (ctx.scalar() != TUPLE_CLOSE)
        return Error.UnclosedTuple;
    ctx.iter.skip();

    defer ctx.padding();
    return Node{ .at = ctx.range(start), .value = Node.Value{ .tuple = arr.items } };
}

fn parseName(ctx: *Ctx) !Node {
    const start = ctx.offset();
    const text = try ctx.readName();

    return Node{
        .at = Node.Range{ .offset = start, .len = text.len },
        .value = Node.Value{ .name = text },
    };
}

inline fn charToDigit(cp: u21) ?u8 {
    const c = @truncate(u8, cp);
    return switch (cp) {
        '0'...'9' => c - '0',
        'A'...'Z' => c - 'A' + 10,
        'a'...'z' => c - 'a' + 10,
        else => null,
    };
}
fn parseInt(ctx: *Ctx, radix: u8) !i128 {
    var x: i128 = 0;
    while (ctx.iter.cur) |cp| {
        if (charToDigit(cp.scalar)) |digit| {
            if (digit >= radix)
                return Error.DigitExceedsRadix;

            x = std.math.mul(i128, x, radix) catch return Error.NumericOverflow;
            x = std.math.add(i128, x, digit) catch return Error.NumericOverflow;
            ctx.iter.skip();
        } else break;
    }
    if (IsNameCp(ctx.scalar()))
        return Error.InvalidDigit;
    return x;
}
fn parseNumber(ctx: *Ctx) !Node {
    const start = ctx.offset();

    const sign = ctx.scalar() == '-';
    if (sign)
        ctx.iter.skip();

    const radix = 10;
    //TODO: read header

    var int: i128 = try ctx.parseInt(radix);
    if (sign)
        int = -int;

    var value = Node.Value{ .integer = int };
    if (ctx.scalar() == DOT) {
        ctx.iter.skip();

        const dstart = ctx.offset();

        const f = try ctx.parseInt(radix);
        var dec: f128 = 0;
        if (f != 0) {
            dec = @intToFloat(f128, f);
            const digits = ctx.range(dstart).len;
            const shift = std.math.powi(i128, digits, radix) catch return Error.NumericOverflow;
            dec /= @intToFloat(f128, shift);
        }
        dec += @intToFloat(f128, int);
        value = Node.Value{ .decimal = dec };
    }

    defer ctx.padding();
    return Node{ .at = ctx.range(start), .value = value };
}

fn readName(ctx: *Ctx) ![]const u8 {
    const name = ctx.iter.readWhile(IsNameCp);
    if (name.len == 0)
        return Error.ExpectName;
    ctx.padding();
    return name;
}
fn padding(ctx: *Ctx) void {
    _ = ctx.iter.readWhile(IsSpace);
}
fn skipPad(ctx: *Ctx) void {
    ctx.iter.skip();
    ctx.padding();
}

fn scalar(ctx: Ctx) u21 {
    return ctx.iter.peek().scalar;
}
fn offset(ctx: Ctx) usize {
    return ctx.iter.peek().offset;
}
fn range(ctx: Ctx, start: usize) Node.Range {
    return Node.Range{ .offset = start, .len = ctx.offset() - start };
}
