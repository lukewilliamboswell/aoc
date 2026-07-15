app [main!] {
	pf: platform "../../basic-cli/platform/main.roc",
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
	answer1 = part1(input) ? |err| SolverFailed(Str.inspect(err))
	answer2 = part2(input) ? |err| SolverFailed(Str.inspect(err))
	Stdout.line!("Part 1: ${answer1}")?
	Stdout.line!("Part 2: ${answer2}")?
	Ok({})
}

part1 : Str -> Try(Str, [NoInventories])
part1 = |input| {
	totals = parse(input).map(List.sum).sort_with(|a, b| U64.compare(b, a))
	highest = totals.first() ? |_| NoInventories
	Ok("The Elf with the highest calories has ${highest.to_str()} kCal")
}

part2 : Str -> Try(Str, [FewerThanThreeElves])
part2 = |input| {
	totals = parse(input).map(List.sum).sort_with(|a, b| U64.compare(b, a))
	match totals {
		[first, second, third, ..] => Ok("Total kCal the Elves are carrying is ${(first + second + third).to_str()}")
		_ => Err(FewerThanThreeElves)
	}
}

parse : Str -> List(List(U64))
parse = |input|
	input
		.trim()
		.split_on("\n\n")
		.map(
			|inventory|
				inventory
					.split_on("\n")
					.fold([], |calories, line| calories.append_if_ok(String.parse_str(String.digits, line))),
		)

example_input = 
	\\1000
	\\2000
	\\3000
	\\
	\\4000
	\\
	\\5000
	\\6000
	\\
	\\7000
	\\8000
	\\9000
	\\
	\\10000

## Part one finds the inventory carrying the most calories.
expect part1(example_input) == Ok("The Elf with the highest calories has 24000 kCal")

## Part two sums the three largest inventories.
expect part2(example_input) == Ok("Total kCal the Elves are carrying is 45000")

## Blank lines separate the inventories.
expect parse(example_input) == [[1000, 2000, 3000], [4000], [5000, 6000], [7000, 8000, 9000], [10000]]
