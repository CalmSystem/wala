const std = @import("std");
const builtin = @import("builtin");
const Pkg = std.build.Pkg;
const string = []const u8;

pub const cache = ".zigmod/deps";

pub fn addAllTo(exe: *std.build.LibExeObjStep) void {
    @setEvalBranchQuota(1_000_000);
    for (packages) |pkg| {
        exe.addPackage(pkg.pkg.?);
    }
    var llc = false;
    var vcpkg = false;
    inline for (std.meta.declarations(package_data)) |decl| {
        const pkg = @as(Package, @field(package_data, decl.name));
        inline for (pkg.system_libs) |item| {
            exe.linkSystemLibrary(item);
            llc = true;
        }
        inline for (pkg.c_include_dirs) |item| {
            exe.addIncludeDir(@field(dirs, decl.name) ++ "/" ++ item);
            llc = true;
        }
        inline for (pkg.c_source_files) |item| {
            exe.addCSourceFile(@field(dirs, decl.name) ++ "/" ++ item, pkg.c_source_flags);
            llc = true;
        }
    }
    if (llc) exe.linkLibC();
    if (builtin.os.tag == .windows and vcpkg) exe.addVcpkgPaths(.static) catch |err| @panic(@errorName(err));
}

pub const Package = struct {
    directory: string,
    pkg: ?Pkg = null,
    c_include_dirs: []const string = &.{},
    c_source_files: []const string = &.{},
    c_source_flags: []const string = &.{},
    system_libs: []const string = &.{},
    vcpkg: bool = false,
};

pub const dirs = struct {
    pub const _root = "";
    pub const _3od6xx3o5jxd = cache ++ "/../..";
    pub const _l21900j9ct9e = cache ++ "/git/github.com/jecolon/ziglyph";
    pub const _xzxo5rnug8wj = cache ++ "/git/github.com/MasterQ32/zig-args";
};

pub const package_data = struct {
    pub const _3od6xx3o5jxd = Package{
        .directory = dirs._3od6xx3o5jxd,
    };
    pub const _l21900j9ct9e = Package{
        .directory = dirs._l21900j9ct9e,
        .pkg = Pkg{ .name = "ziglyph", .path = .{ .path = dirs._l21900j9ct9e ++ "/src/ziglyph.zig" }, .dependencies = null },
    };
    pub const _xzxo5rnug8wj = Package{
        .directory = dirs._xzxo5rnug8wj,
        .pkg = Pkg{ .name = "args", .path = .{ .path = dirs._xzxo5rnug8wj ++ "/args.zig" }, .dependencies = null },
    };
    pub const _root = Package{
        .directory = dirs._root,
    };
};

pub const packages = &[_]Package{
    package_data._l21900j9ct9e,
    package_data._xzxo5rnug8wj,
};

pub const pkgs = struct {
    pub const ziglyph = package_data._l21900j9ct9e;
    pub const args = package_data._xzxo5rnug8wj;
};

pub const imports = struct {
    pub const ziglyph = @import(".zigmod/deps/git/github.com/jecolon/ziglyph/src/ziglyph.zig");
    pub const args = @import(".zigmod/deps/git/github.com/MasterQ32/zig-args/args.zig");
};
