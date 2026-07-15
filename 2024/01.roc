app [main!] {
	pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.21.0-rc3/Cv3oh6aPxqZxDeh1dgnXPjZnYsoqQMvEawa6TUc71sxJ.tar.zst",
	parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/1.0.2/FrnJ4RGDKpQyoDyESNoBwFNviY4ZGbMVLnUjW9tvSRjk.tar.zst",
}

import pf.OsStr
import pf.Stdin
import pf.Stdout
import parser.Parser exposing [Parser]
import parser.String

main! : List(OsStr) => Try({}, _)
main! = |_| {
	bytes = Stdin.read_to_end!()?
	input = Str.from_utf8(bytes) ? |err| InvalidUtf8(err)
	answer1 = part1(input) ? |err| SolverFailed(Str.inspect(err))
	answer2 = part2(input) ? |err| SolverFailed(Str.inspect(err))
	Stdout.line!("Part 1: ${answer1}")?
	Stdout.line!("Part 2: ${answer2}")?
	Ok({})
}

part1 : Str -> Try(Str, _)
part1 = |input| {
	numbers = String.parse_str(parse_location_ids.sep_by(String.codeunit('\n')), input.trim())?
	{ first, second } = split_and_sort(numbers)
	distance = calc_distance(first, second, 0)
	Ok("The total distance between the lists is ${distance.to_str()}.")
}

## Part 1 calculates the sample location distance.
expect part1(example_input) == Ok("The total distance between the lists is 11.")

part2 : Str -> Try(Str, _)
part2 = |input| {
	numbers = String.parse_str(parse_location_ids.sep_by(String.codeunit('\n')), input.trim())?
	{ first, second } = split_and_sort(numbers)
	similarity = calc_similarity(first, second, 0)
	Ok("The similarity score is ${similarity.to_str()}.")
}

## Part 2 calculates the sample similarity score.
expect part2(example_input) == Ok("The similarity score is 31.")

example_input = 
	\\3   4
	\\4   3
	\\2   5
	\\1   3
	\\3   9
	\\3   3

parse_location_ids : Parser(String.Utf8, { first : U64, second : U64 })
parse_location_ids = 
	Parser.const(|first| |second| { first, second })
		.keep(String.digits)
		.skip(String.string("   "))
		.keep(String.digits)

## A location row parses both identifiers.
expect String.parse_str(parse_location_ids, "3   4") == Ok({ first: 3, second: 4 })

split_and_sort : List({ first : U64, second : U64 }) -> { first : List(U64), second : List(U64) }
split_and_sort = |numbers| {
	first = numbers.map(|item| item.first).sort_with(U64.compare)
	second = numbers.map(|item| item.second).sort_with(U64.compare)
	{ first, second }
}

calc_distance : List(U64), List(U64), U64 -> U64
calc_distance = |first, second, score| {
	match (first, second) {
		([], []) => score
		([a, .. as rest_a], [b, .. as rest_b]) => calc_distance(rest_a, rest_b, score + a.abs_diff(b))
		_ => {
			crash "expected input lists to be the same length"
		}
	}
}

calc_similarity : List(U64), List(U64), U64 -> U64
calc_similarity = |first, second, score| {
	match first {
		[] => score
		[a, .. as rest] => {
			count = second.count_if(|b| a == b)
			calc_similarity(rest, second, score + a * count)
		}
	}
}
