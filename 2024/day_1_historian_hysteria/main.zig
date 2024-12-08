const std = @import("std");

const LocationID = usize;

const ParsedLocationID = struct {
    number: LocationID,
    scanned: usize,
};

fn parseLocationID(index: usize, line: []const u8) ParsedLocationID {
    var number: LocationID = 0;
    var scanned: usize = 0;
    while (index + scanned < line.len) {
        const offset = index + scanned;
        if (!std.ascii.isDigit(line[offset])) {
            break;
        }
        const d = @as(usize, line[offset] - '0');
        number = number * 10 + d;
        scanned += 1;
    }
    return .{ .number = number, .scanned = scanned };
}

fn consumeWhitespace(index: usize, line: []const u8) usize {
    var scanned: usize = 0;
    while (index + scanned < line.len and line[index + scanned] == ' ') {
        scanned += 1;
    }
    return scanned;
}

const LocationIDPair = struct {
    left: LocationID,
    right: LocationID,
};

fn parseLine(line: []const u8) LocationIDPair {
    var index: usize = 0;
    const left = parseLocationID(index, line);
    index += left.scanned;
    index += consumeWhitespace(index, line);
    const right = parseLocationID(index, line);
    return .{ .left = left.number, .right = right.number };
}

const LocationLists = struct {
    allocator: std.mem.Allocator,
    left: std.ArrayList(LocationID),
    right: std.ArrayList(LocationID),

    const Self = @This();
    fn init(allocator: std.mem.Allocator) Self {
        return .{
            .allocator = allocator,
            .left = std.ArrayList(usize).init(allocator),
            .right = std.ArrayList(usize).init(allocator),
        };
    }

    fn deinit(self: *Self) void {
        self.left.deinit();
        self.right.deinit();
    }

    fn addLocations(self: *Self, left_location: LocationID, right_location: LocationID) !void {
        try self.left.append(left_location);
        try self.right.append(right_location);
    }

    fn sort(self: Self) void {
        std.mem.sort(LocationID, self.left.items, {}, std.sort.asc(LocationID));
        std.mem.sort(LocationID, self.right.items, {}, std.sort.asc(LocationID));
    }

    fn total_distance(self: Self) usize {
        var d: usize = 0;
        for (self.left.items, 0..) |left, i| {
            const right = self.right.items[i];
            d += if (left < right) right - left else left - right;
        }
        return d;
    }

    fn total_similarity_score(self: Self) !usize {
        var right_counts = std.AutoHashMap(LocationID, usize).init(self.allocator);
        defer right_counts.deinit();

        for (self.right.items) |right| {
            const entry = try right_counts.getOrPut(right);
            entry.value_ptr.* = if (entry.found_existing) entry.value_ptr.* + 1 else 1;
        }

        var score: usize = 0;
        for (self.left.items) |left| {
            score += left * (right_counts.get(left) orelse 0);
        }

        return score;
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

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
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

    var location_lists = LocationLists.init(allocator);
    defer location_lists.deinit();

    var it = std.mem.splitScalar(u8, buffer, '\n');
    while (it.next()) |line| {
        const numbers = parseLine(line);
        try location_lists.addLocations(numbers.left, numbers.right);
    }
    location_lists.sort();
    const distance = location_lists.total_distance();
    try stdout.print("{d}\n", .{distance});

    const similarity_score = try location_lists.total_similarity_score();
    try stdout.print("{d}\n", .{similarity_score});
}
