app [main!] {
	pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.21.0-rc3/Cv3oh6aPxqZxDeh1dgnXPjZnYsoqQMvEawa6TUc71sxJ.tar.zst",
	parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/1.0.2/FrnJ4RGDKpQyoDyESNoBwFNviY4ZGbMVLnUjW9tvSRjk.tar.zst",
}

import pf.OsStr
import pf.Stdin
import pf.Stdout
import parser.String

main! : List(OsStr) => Try({}, _)
main! = |_args| {
	bytes = Stdin.read_to_end!()?
	input = Str.from_utf8(bytes) ? |err| InvalidUtf8(err)
	Stdout.line!("Part 1: ${part1(input)}")?
	Stdout.line!("Part 2: ${part2(input)}")?
	Ok({})
}

part1 : Str -> Str
part1 = |input| {
	answer = count_depth_increases(parse_input(input))
	"The number of depth increases is ${answer.to_str()}"
}

part2 : Str -> Str
part2 = |input| {
	answer = count_depth_increases(sliding_window(parse_input(input)))
	"The number of depth increases is ${answer.to_str()}"
}

parse_input : Str -> List(U64)
parse_input = |content|
	content
		.split_on("\n")
		.fold([], |numbers, line| numbers.append_if_ok(String.parse_str(String.digits, line)))

count_depth_increases : List(U64) -> U64
count_depth_increases = |depths|
	match depths {
		[] => 0
		[first, .. as rest] => rest.fold(
			{ last: first, count: 0 },
			|state, depth| {
				count = if depth > state.last state.count + 1 else state.count
				{ last: depth, count }
			},
		).count
	}

sliding_window : List(U64) -> List(U64)
sliding_window = |depths|
	match depths {
		[a, b, c, .. as rest] => [a + b + c].concat(sliding_window([b, c].concat(rest)))
		_ => []
	}

## Non-numeric input lines are ignored.
expect parse_input("not-a-number\n123\n345\n678\n") == [123, 345, 678]

## Depth increases are counted relative to the previous reading.
expect count_depth_increases([199, 200, 208, 210, 200, 207, 240, 269, 260, 263]) == 7

## Sliding windows contain the sums of each three adjacent readings.
expect sliding_window([1, 2, 3, 4, 5, 6]) == [6, 9, 12, 15]
