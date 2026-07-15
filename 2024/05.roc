app [main!] {
	pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.21.0-rc3/Cv3oh6aPxqZxDeh1dgnXPjZnYsoqQMvEawa6TUc71sxJ.tar.zst",
	parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/1.0.2/FrnJ4RGDKpQyoDyESNoBwFNviY4ZGbMVLnUjW9tvSRjk.tar.zst",
}

import pf.OsStr
import pf.Stdin
import pf.Stdout
import parser.Parser exposing [Parser]
import parser.String

Rule : { before : U64, after : U64 }

Input : { rules : List(Rule), updates : List(List(U64)) }

main! : List(OsStr) => Try({}, _)
main! = |_| {
	input = Str.from_utf8(Stdin.read_to_end!()?) ? |err| InvalidUtf8(err)
	answer1 = part1(input) ? |err| SolverFailed(Str.inspect(err))
	answer2 = part2(input) ? |err| SolverFailed(Str.inspect(err))
	Stdout.line!("Part 1: ${answer1}")?
	Stdout.line!("Part 2: ${answer2}")?
	Ok({})
}

part1 : Str -> Try(Str, _)
part1 = |input| {
	{ rules, updates } = parse(input.trim())?
	total = updates.keep_if(|update| rules.all(|rule| check_rule(update, rule))).map(get_middle).sum()
	Ok(total.to_str())
}

## Part one sums middle pages from updates already in the right order.
expect part1(example_input) == Ok("143")

part2 : Str -> Try(Str, _)
part2 = |input| {
	{ rules, updates } = parse(input.trim())?
	total = updates
		.keep_if(|update| rules.any(|rule| !check_rule(update, rule)))
		.map(|update| apply_rules(update, rules))
		.map(get_middle)
		.sum()
	Ok(total.to_str())
}

## Part two reorders invalid updates before summing their middle pages.
expect part2(example_input) == Ok("123")

check_rule : List(U64), Rule -> Bool
check_rule = |update, { before, after }| match update.keep_if(|number| number == before or number == after) {
	[first, second] => first == before and second == after
	_ => Bool.True
}

## A correctly ordered update satisfies a relevant rule.
expect check_rule([75, 47, 61, 53, 29], { before: 47, after: 53 })

reorder_rule : List(U64), Rule -> [Swapped(List(U64)), NoChange]
reorder_rule = |update, { before, after }| {
	positions = { before: find_index(update, before, 0), after: find_index(update, after, 0) }
	match (positions.before, positions.after) {
		(Some(before_index), Some(after_index)) if before_index > after_index => Swapped(update.swap(before_index, after_index) ?? update)
		_ => NoChange
	}
}

find_index : List(U64), U64, U64 -> [Some(U64), None]
find_index = |numbers, wanted, index| match numbers {
	[] => None
	[first, ..] if first == wanted => Some(index)
	[_, .. as rest] => find_index(rest, wanted, index + 1)
}

## A violated rule swaps the two relevant pages.
expect reorder_rule([75, 53, 61, 47, 29], { before: 47, after: 53 }) == Swapped([75, 47, 61, 53, 29])

apply_rules : List(U64), List(Rule) -> List(U64)
apply_rules = |update, rules| {
	outcome = rules.fold_until(
		NoChange,
		|_, rule| match reorder_rule(update, rule) {
			Swapped(new) => Break(Swapped(new))
			NoChange => Continue(NoChange)
		},
	)
	match outcome {
		Swapped(new) => apply_rules(new, rules)
		NoChange => update
	}
}

## Rule application repeats until the whole update is ordered.
expect apply_rules(
	[75, 47, 61, 53, 29],
	[{ before: 61, after: 47 }, { before: 99, after: 47 }, { before: 47, after: 75 }],
) == [61, 47, 75, 53, 29]

get_middle : List(U64) -> U64
get_middle = |numbers| match numbers {
	[middle] => middle
	[_, .., _] => get_middle(numbers.drop_first(1).drop_last(1))
	[] => {
		crash "expected an odd, non-empty update"
	}
}

## The middle page is selected from an odd-length update.
expect get_middle([1, 2, 3, 4, 5]) == 3

parse : Str -> Try(Input, _)
parse = |input| String.parse_str(parse_input, input)

parse_rule : Parser(String.Utf8, Rule)
parse_rule = Parser.const(|before| |after| { before, after })
	.keep(String.digits)
	.skip(String.codeunit('|'))
	.keep(String.digits)

## Rule parsing reads the before and after page numbers.
expect String.parse_str(parse_rule, "47|53") == Ok({ before: 47, after: 53 })

parse_update : Parser(String.Utf8, List(U64))
parse_update = String.digits.sep_by(String.codeunit(','))

## Update parsing reads comma-separated page numbers.
expect String.parse_str(parse_update, "75,47,61,53,29") == Ok([75, 47, 61, 53, 29])

parse_input : Parser(String.Utf8, Input)
parse_input = Parser.const(|rules| |updates| { rules, updates })
	.keep(parse_rule.sep_by(String.codeunit('\n')))
	.skip(String.string("\n\n"))
	.keep(parse_update.sep_by(String.codeunit('\n')))

example_input = 
	\\47|53
	\\97|13
	\\97|61
	\\97|47
	\\75|29
	\\61|13
	\\75|53
	\\29|13
	\\97|29
	\\53|29
	\\61|53
	\\97|53
	\\61|29
	\\47|13
	\\75|47
	\\97|75
	\\47|61
	\\75|61
	\\47|29
	\\75|13
	\\53|13
	\\
	\\75,47,61,53,29
	\\97,61,53,29,13
	\\75,29,13
	\\75,97,47,61,53
	\\61,13,29
	\\97,13,75,29,47
