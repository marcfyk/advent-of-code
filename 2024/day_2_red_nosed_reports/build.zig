const std = @import("std");

pub fn build(b: *std.Build) void {
    const exe = b.addExecutable(.{
        .name = "day_2_red_nosed_reports",
        .root_source_file = b.path("main.zig"),
        .target = b.host,
    });
    b.installArtifact(exe);
}
