app [main!] {
	pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.21.0-rc3/Cv3oh6aPxqZxDeh1dgnXPjZnYsoqQMvEawa6TUc71sxJ.tar.zst",
	parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/1.0.2/FrnJ4RGDKpQyoDyESNoBwFNviY4ZGbMVLnUjW9tvSRjk.tar.zst",
}

import pf.OsStr
import pf.Stdin
import pf.Stdout
import parser.Parser exposing [Parser]
import parser.String

AssignmentPair : {
	start_a : U64,
	end_a : U64,
	start_b : U64,
	end_b : U64,
}

main! : List(OsStr) => Try({}, _)
main! = |_args| {
	bytes = Stdin.read_to_end!()?
	input = Str.from_utf8(bytes) ? |err| InvalidUtf8(err)
	answer1 = part1(input) ? |err| SolverFailed(Str.inspect(err))
	answer2 = part2(input) ? |err| SolverFailed(Str.inspect(err))
	Stdout.line!("Part 1: ${answer1.to_str()}")?
	Stdout.line!("Part 2: ${answer2.to_str()}")?
	Ok({})
}

part1 : Str -> Try(U64, _)
part1 = |input| {
	pairs = parse_input(input)?
	Ok(pairs.count_if(is_fully_contained))
}

part2 : Str -> Try(U64, _)
part2 = |input| {
	pairs = parse_input(input)?
	Ok(pairs.count_if(is_any_overlap))
}

parse_input : Str -> Try(List(AssignmentPair), _)
parse_input = |input|
	String.parse_str(assignment_pair_parser.sep_by(String.codeunit('\n')), input.trim())

assignment_pair_parser : Parser(String.Utf8, AssignmentPair)
assignment_pair_parser = 
	Parser.const(|start_a| |end_a| |start_b| |end_b| { start_a, end_a, start_b, end_b })
		.keep(String.digits)
		.skip(String.codeunit('-'))
		.keep(String.digits)
		.skip(String.codeunit(','))
		.keep(String.digits)
		.skip(String.codeunit('-'))
		.keep(String.digits)

is_fully_contained : AssignmentPair -> Bool
is_fully_contained = |{ start_a, end_a, start_b, end_b }|
	(start_a >= start_b and end_a <= end_b) or (start_b >= start_a and end_b <= end_a)

is_any_overlap : AssignmentPair -> Bool
is_any_overlap = |{ start_a, end_a, start_b, end_b }|
	start_a <= end_b and start_b <= end_a

assignment_pair_to_str : AssignmentPair -> Str
assignment_pair_to_str = |{ start_a, end_a, start_b, end_b }|
	"${start_a.to_str()}-${end_a.to_str()},${start_b.to_str()}-${end_b.to_str()}"

example = 
	\\2-4,6-8
	\\2-3,4-5
	\\5-7,7-9
	\\2-8,3-7
	\\6-6,4-6
	\\2-6,4-8

## The sample contains two fully contained assignment pairs.
expect part1(example)? == 2

## The sample contains four overlapping assignment pairs.
expect part2(example)? == 4

## Assignment pairs retain their compact textual representation.
expect assignment_pair_to_str({ start_a: 2, end_a: 8, start_b: 3, end_b: 7 }) == "2-8,3-7"
