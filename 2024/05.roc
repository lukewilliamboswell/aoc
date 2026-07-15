app [main!] {
	pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.21.0-rc4/FvCh4vdqm3nBY6DWEfZ8RuGCVfjuMY43HA8KSNk9qVDn.tar.zst",
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

part2 : Str -> Try(Str, [CyclicRules, ParsingFailure(Str), ParsingIncomplete(Str)])
part2 = |input| {
	{ rules, updates } = parse(input.trim())?
	initial : Try(U64, [CyclicRules])
	initial = Ok(0)
	total = updates.keep_if(|update| rules.any(|rule| !check_rule(update, rule))).fold(
		initial,
		|result, update| {
			sum = result?
			ordered = order_update(update, rules)?
			Ok(sum + get_middle(ordered))
		},
	)?
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

order_update : List(U64), List(Rule) -> Try(List(U64), [CyclicRules])
order_update = |update, rules| order_pages(update, rules, [])

order_pages : List(U64), List(Rule), List(U64) -> Try(List(U64), [CyclicRules])
order_pages = |remaining, rules, ordered| match remaining {
	[] => Ok(ordered)
	_ => match remaining.find_first(
		|page|
			!rules.any(|rule| rule.after == page and remaining.contains(rule.before)),
	) {
		Ok(next) => order_pages(remaining.keep_if(|page| page != next), rules, ordered.append(next))
		Err(_) => Err(CyclicRules)
	}
}

## Ordering uses the transitive rule graph rather than repeated pair swaps.
expect order_update([3, 2, 1], [{ before: 1, after: 2 }, { before: 2, after: 3 }]) == Ok([1, 2, 3])

## Cyclic ordering rules return a structured error.
expect order_update([1, 2], [{ before: 1, after: 2 }, { before: 2, after: 1 }]) == Err(CyclicRules)

get_middle : List(U64) -> U64
get_middle = |numbers| {
	if numbers.is_empty() or numbers.len() % 2 == 0 {
		crash "expected an odd, non-empty update"
	}
	numbers.get(numbers.len() / 2) ?? {
		crash "middle index must be in bounds"
	}
}

## The middle page is selected from an odd-length update.
expect get_middle([1, 2, 3, 4, 5]) == 3

parse : Str -> Try(Input, _)
parse = |input| String.parse_str(parse_input, input)

parse_rule : Parser(String.Utf8, Rule)
parse_rule = {
	before: String.digits.skip(String.codeunit('|')),
	after: String.digits,
}.Parser

## Rule parsing reads the before and after page numbers.
expect String.parse_str(parse_rule, "47|53") == Ok({ before: 47, after: 53 })

parse_update : Parser(String.Utf8, List(U64))
parse_update = String.digits.sep_by(String.codeunit(','))

## Update parsing reads comma-separated page numbers.
expect String.parse_str(parse_update, "75,47,61,53,29") == Ok([75, 47, 61, 53, 29])

parse_input : Parser(String.Utf8, Input)
parse_input = {
	rules: parse_rule.sep_by(String.codeunit('\n')).skip(String.string("\n\n")),
	updates: parse_update.sep_by(String.codeunit('\n')),
}.Parser

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
