const std = @import("std");

const Report = struct {
    levels: std.ArrayList(usize),

    const Self = @This();

    const Ord = enum { asc, desc, eq };

    fn init(allocator: std.mem.Allocator) Self {
        return .{
            .levels = std.ArrayList(usize).init(allocator),
        };
    }

    fn deinit(self: Self) void {
        self.levels.deinit();
    }

    fn pairwiseCheck(ord: Ord, x: usize, y: usize) bool {
        const diff = if (x > y) x - y else y - x;
        if (diff < 1 or diff > 3) {
            return false;
        }
        switch (ord) {
            Ord.eq => return false,
            Ord.asc => return x < y,
            Ord.desc => return x > y,
        }
    }

    fn getOrd(self: Self, ignore_index: ?usize) ?Ord {
        if (self.levels.items.len < 2) {
            return null;
        }
        var first = self.levels.items[0];
        var second = self.levels.items[1];
        if (ignore_index != null) {
            if (self.levels.items.len < 3) {
                return null;
            }
            if (ignore_index.? == 0) {
                first = self.levels.items[1];
                second = self.levels.items[2];
            } else if (ignore_index.? == 1) {
                second = self.levels.items[2];
            }
        }
        return if (first < second) Ord.asc else if (first > second) Ord.desc else Ord.eq;
    }

    fn validate(self: Self, ignore_index: ?usize) bool {
        std.debug.assert(ignore_index == null or (0 <= ignore_index.? and ignore_index.? < self.levels.items.len));
        const ord = self.getOrd(ignore_index);
        if (ord == null) {
            return true;
        }

        for (0..self.levels.items.len - 1) |i| {
            var index_to_compare = i + 1;
            if (ignore_index != null) {
                if (ignore_index.? == i) {
                    continue;
                } else if (ignore_index.? == index_to_compare) {
                    if (i + 2 >= self.levels.items.len) {
                        break;
                    }
                    index_to_compare = i + 2;
                }
            }

            if (!pairwiseCheck(ord.?, self.levels.items[i], self.levels.items[index_to_compare])) {
                return false;
            }
        }
        return true;
    }

    fn isSafe(self: *Self) bool {
        return self.validate(null);
    }

    fn isSafeWithTolerance(self: Self) bool {
        for (0..self.levels.items.len) |i| {
            if (self.validate(i)) {
                return true;
            }
        }
        return false;
    }
};

fn parseArgs() ![]const u8 {
    var args = std.process.args();
    _ = args.skip();
    const input_arg = args.next();
    if (input_arg == null) {
        try std.io.getStdErr().writer().print("no input file given\n", .{});
        std.process.exit(1);
    }
    return input_arg.?;
}

fn parseLine(allocator: std.mem.Allocator, line: []const u8) !Report {
    var report = Report.init(allocator);
    var it = std.mem.tokenizeScalar(u8, line, ' ');
    var index: usize = 0;
    while (it.next()) |number| : (index += 1) {
        const level = try std.fmt.parseInt(usize, number, 10);
        try report.levels.append(level);
    }
    return report;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();
    const stdout = std.io.getStdOut().writer();

    const input = try parseArgs();

    const file = try std.fs.cwd().openFile(input, .{});
    defer file.close();

    const file_size = try file.getEndPos();
    const buffer = try allocator.alloc(u8, file_size);
    defer allocator.free(buffer);
    _ = try file.read(buffer);

    var it = std.mem.tokenizeScalar(u8, buffer, '\n');
    var safe_reports: usize = 0;
    var safe_reports_with_tolerance: usize = 0;
    while (it.next()) |line| {
        var report = try parseLine(allocator, line);
        defer report.deinit();
        safe_reports += if (report.isSafe()) 1 else 0;
        safe_reports_with_tolerance += if (report.isSafeWithTolerance()) 1 else 0;
    }
    try stdout.print("part one: {d}\npart two: {d}\n", .{ safe_reports, safe_reports_with_tolerance });
}
