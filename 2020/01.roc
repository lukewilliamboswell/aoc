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
	answer1 = part1(input) ? |err| SolverFailed(Str.inspect(err))
	answer2 = part2(input) ? |err| SolverFailed(Str.inspect(err))
	Stdout.line!("Part 1: ${answer1}")?
	Stdout.line!("Part 2: ${answer2}")?
	Ok({})
}

part1 : Str -> Try(Str, [SolveFailed(Str)])
part1 = |input| {
	numbers = parse(input) ? |err| SolveFailed(Str.inspect(err))
	{ x, y } = find_pair(numbers) ? |err| SolveFailed(Str.inspect(err))
	Ok("${x.to_str()} * ${y.to_str()} = ${(x * y).to_str()}")
}

part2 : Str -> Try(Str, [SolveFailed(Str)])
part2 = |input| {
	numbers = parse(input) ? |err| SolveFailed(Str.inspect(err))
	{ x, y, z } = find_triple(numbers) ? |err| SolveFailed(Str.inspect(err))
	Ok("${x.to_str()} * ${y.to_str()} * ${z.to_str()} = ${(x * y * z).to_str()}")
}

parse : Str -> Try(List(U64), _)
parse = |input| String.parse_str(String.digits.sep_by(String.codeunit('\n')), input.trim())

find_pair : List(U64) -> Try({ x : U64, y : U64 }, [NoPair])
find_pair = |numbers| Ok(find_pair_sum(numbers, 2020) ? |_| NoPair)

find_pair_sum : List(U64), U64 -> Try({ x : U64, y : U64 }, [NotFound])
find_pair_sum = |numbers, target| match numbers {
	[] => Err(NotFound)
	[x, .. as rest] if x <= target => match rest.find_first(|y| x + y == target) {
		Ok(y) => Ok({ x, y })
		Err(_) => find_pair_sum(rest, target)
	}
	[_, .. as rest] => find_pair_sum(rest, target)
}

find_triple : List(U64) -> Try({ x : U64, y : U64, z : U64 }, [NoTriple])
find_triple = |numbers| Ok(find_triple_sum(numbers, 2020) ? |_| NoTriple)

find_triple_sum : List(U64), U64 -> Try({ x : U64, y : U64, z : U64 }, [NotFound])
find_triple_sum = |numbers, target| match numbers {
	[] => Err(NotFound)
	[x, .. as rest] if x <= target => match find_pair_sum(rest, target - x) {
		Ok({ x: y, y: z }) => Ok({ x, y, z })
		Err(_) => find_triple_sum(rest, target)
	}
	[_, .. as rest] => find_triple_sum(rest, target)
}

example = 
	\\1721
	\\979
	\\366
	\\299
	\\675
	\\1456

## The sample pair sums to 2020.
expect part1(example) == Ok("1721 * 299 = 514579")

## The sample triple sums to 2020.
expect part2(example) == Ok("979 * 366 * 675 = 241861950")

## Input lines parse as unsigned integers.
expect parse(example)? == [1721, 979, 366, 299, 675, 1456]

## Expense entries cannot be reused to form a pair.
expect find_pair([1010]) == Err(NoPair)
