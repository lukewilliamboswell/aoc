app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.21.0-rc4/FvCh4vdqm3nBY6DWEfZ8RuGCVfjuMY43HA8KSNk9qVDn.tar.zst" }

import pf.OsStr
import pf.Stdin
import pf.Stdout

Rucksack : { left : List(U8), right : List(U8) }

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

part1 : Str -> Try(U64, [NoCommonItem])
part1 = |input| sum_compartment_priorities(parse_input(input), 0)

part2 : Str -> Try(U64, [IncompleteGroup, NoCommonItem])
part2 = |input| sum_group_priorities(parse_input(input), 0)

parse_input : Str -> List(Rucksack)
parse_input = |input|
	input.trim().split_on("\n").map(parse_rucksack)

parse_rucksack : Str -> Rucksack
parse_rucksack = |line| {
	items = line.to_utf8()
	{ before, others } = items.split_at(items.len() / 2)
	{ left: before, right: others }
}

sum_compartment_priorities : List(Rucksack), U64 -> Try(U64, [NoCommonItem])
sum_compartment_priorities = |rucksacks, total|
	match rucksacks {
		[] => Ok(total)
		[first, .. as rest] => {
			common = Set.intersection(Set.from_list(first.left), Set.from_list(first.right)).to_list().first() ? |_| NoCommonItem
			sum_compartment_priorities(rest, total + item_priority(common))
		}
	}

sum_group_priorities : List(Rucksack), U64 -> Try(U64, [IncompleteGroup, NoCommonItem])
sum_group_priorities = |rucksacks, total|
	match rucksacks {
		[] => Ok(total)
		[first, second, third, .. as rest] => {
			first_items = Set.from_list(first.left.concat(first.right))
			second_items = Set.from_list(second.left.concat(second.right))
			third_items = Set.from_list(third.left.concat(third.right))
			common = Set.intersection(Set.intersection(first_items, second_items), third_items).to_list().first() ? |_| NoCommonItem
			sum_group_priorities(rest, total + item_priority(common))
		}
		_ => Err(IncompleteGroup)
	}

item_priority : U8 -> U64
item_priority = |item| {
	if item >= 'a' and item <= 'z' {
		U8.to_u64(item - 'a') + 1
	} else {
		U8.to_u64(item - 'A') + 27
	}
}

example = 
	\\vJrwpWtwJgWrhcsFMMfFFhFp
	\\jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
	\\PmmdzqPrVvPwwTWBwg
	\\wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
	\\ttgJtRGJQctTZtZT
	\\CrZsJsPPZsGzwwsLwLmpwMDw

## The sample duplicate item priorities sum to 157.
expect part1(example)? == 157

## The sample badge priorities sum to 70.
expect part2(example)? == 70

## Letter priorities run from lowercase a through uppercase Z.
expect item_priority('a') == 1 and item_priority('z') == 26 and item_priority('A') == 27 and item_priority('Z') == 52
