const u = @import("../util.zig");

/// See https://github.com/WebAssembly/tool-conventions/blob/main/Linking.md
/// Custom section after data
version: u32 = 2,
subsections: []Sub = &[_]Sub{},

pub const Sub = struct {
    //TODO: initFuncs, symbolTable, ...
    type: u8,
    len: u32,
    data: u.Bin,
};

/// Custom sections reloc.* after linking one
pub const Reloc = struct {
    /// index of the target section
    section: u32,
    entries: []Entry,

    pub const Entry = struct {
        type: Type,
        /// varuint32
        offset: u32,
        /// varuint32
        index: u32,
        /// varuint32
        addend: i32 = 0,
    };
    pub const Type = enum(u8) {
        /// a function index encoded as a 5-byte varuint32.
        /// Used for the immediate argument of a call instruction.
        functionIndexLeb = 0,
        /// a function table index encoded as a 5-byte varint32.
        /// Used to refer to the immediate argument of a i32.const instruction,
        /// e.g. taking the address of a function.
        tableIndexSleb = 1,
        /// a function table index encoded as a uint32,
        /// e.g. taking the address of a function in a static data initializer.
        tableIndexI32 = 2,
        /// a linear memory index encoded as a 5-byte varuint32.
        /// Used for the immediate argument of a load or store instruction,
        /// e.g. directly loading from or storing to a C++ global.
        memoryAddrLeb = 3,
        /// a linear memory index encoded as a 5-byte varint32.
        /// Used for the immediate argument of a i32.const instruction,
        /// e.g. taking the address of a C++ global.
        memoryAddrSleb = 4,
        /// a linear memory index encoded as a uint32,
        /// e.g. taking the address of a C++ global in a static data initializer.
        memoryAddrI32 = 5,
        /// a type index encoded as a 5-byte varuint32,
        /// e.g. the type immediate in a call_indirect.
        typeIndexLeb = 6,
        /// a global index encoded as a 5-byte varuint32,
        /// e.g. the index immediate in a get_global.
        globalIndexLeb = 7,
        /// a byte offset within code section for the specific function encoded as a uint32.
        /// The offsets start at the actual function code excluding its size field.
        functionOffsetI32 = 8,
        /// a byte offset from start of the specified section encoded as a uint32.
        sectionOffsetI32 = 9,
        /// an event index encoded as a 5-byte varuint32.
        /// Used for the immediate argument of a throw and if_except instruction.
        eventIndexLeb = 10,
        /// a global index encoded as uint32.
        globalIndexI32 = 13,
        //NOTE: the 64bit relocations are not yet stable and therefore, subject to change.
        /// the 64-bit counterpart of MEMORY_ADDR_LEB.
        /// A 64-bit linear memory index encoded as a 10-byte varuint64,
        /// Used for the immediate argument of a load or store instruction on a 64-bit linear memory array.
        memoryAddrLeb64 = 14,
        /// the 64-bit counterpart of MEMORY_ADDR_SLEB.
        /// A 64-bit linear memory index encoded as a 10-byte varint64.
        /// Used for the immediate argument of a i64.const instruction.
        memoryAddrSleb64 = 15,
        /// the 64-bit counterpart of MEMORY_ADDR.
        /// A 64-bit linear memory index encoded as a uint64,
        /// taking the 64-bit address of a C++ global in a static data initializer.
        memoryAddrI64 = 16,
        /// the 64-bit counterpart of TABLE_INDEX_SLEB.
        /// A function table index encoded as a 10-byte varint64.
        /// Used to refer to the immediate argument of a i64.const instruction,  taking the address of a function in Wasm64.
        tableIndexSleb64 = 18,
        /// the 64-bit counterpart of TABLE_INDEX_I32.
        /// A function table index encoded as a uint64,  taking the address of a function in a static data initializer.
        tableIndexI64 = 19,
        /// a table number encoded as a 5-byte varuint32.
        /// Used for the table immediate argument in the table.* instructions.
        tableNumberLeb = 20,
    };
};

//MAYBE: TargetFeature
