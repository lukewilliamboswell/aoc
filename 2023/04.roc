app [main!] {
	pf: platform "../../basic-cli/platform/main.roc",
	parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/1.0.2/FrnJ4RGDKpQyoDyESNoBwFNviY4ZGbMVLnUjW9tvSRjk.tar.zst",
}

import pf.OsStr
import pf.Stdin
import pf.Stdout
import parser.Parser exposing [Parser]
import parser.String

Card : { id : U64, winning : List(U64), picks : List(U64) }

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
	cards = String.parse_str(parse_card.sep_by(String.codeunit('\n')), input.trim())?
	Ok("The scratch cards are worth a total of ${cards.map(score_card).sum().to_str()} points.")
}

## Part one scores the example scratchcards.
expect part1(example_input) == Ok("The scratch cards are worth a total of 13 points.")

part2 : Str -> Try(Str, _)
part2 = |input| {
	cards = String.parse_str(parse_card.sep_by(String.codeunit('\n')), input.trim())?
	initial_counts = cards.map(|_| 1)
	counts = count_all_cards(cards, initial_counts, 0)
	Ok("The total number is ${counts.sum().to_str()} scratchcards.")
}

## Part two counts won copies of subsequent scratchcards.
expect part2(example_input) == Ok("The total number is 30 scratchcards.")

parse_card : Parser(String.Utf8, Card)
parse_card = {
	spaces = String.codeunit(' ').one_or_more()
	Parser.const(|id| |winning| |picks| { id, winning, picks })
		.skip(String.string("Card"))
		.skip(spaces)
		.keep(String.digits)
		.skip(String.codeunit(':'))
		.skip(spaces)
		.keep(String.digits.sep_by(spaces))
		.skip(String.string(" |"))
		.skip(spaces)
		.keep(String.digits.sep_by(spaces))
}

example_card = {
	id: 1,
	winning: [41, 48, 83, 86, 17],
	picks: [83, 86, 6, 31, 17, 9, 48, 53],
}

## Card parsing handles variable-width whitespace between numbers.
expect String.parse_str(parse_card, "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53") == Ok(example_card)

count_wins : Card -> U64
count_wins = |card| card.picks.count_if(|pick| card.winning.contains(pick))

score_card : Card -> U64
score_card = |card| {
	wins = count_wins(card)
	if wins == 0 {
		0
	} else {
		pow_two(wins - 1, 1)
	}
}

pow_two : U64, U64 -> U64
pow_two = |power, result| if power == 0 {
	result
} else {
	pow_two(power - 1, result * 2)
}

## A card with four matches is worth eight points.
expect score_card(example_card) == 8

count_all_cards : List(Card), List(U64), U64 -> List(U64)
count_all_cards = |cards, counts, index| match cards.drop_first(index) {
	[] => counts
	[card, ..] => {
		current_count = get_count(counts, index)
		updated = add_copies(counts, index + 1, count_wins(card), current_count)
		count_all_cards(cards, updated, index + 1)
	}
}

add_copies : List(U64), U64, U64, U64 -> List(U64)
add_copies = |counts, index, remaining, amount| {
	if remaining == 0 or index >= counts.len() {
		counts
	} else {
		updated = counts.update(index, |count| count + amount) ?? counts
		add_copies(updated, index + 1, remaining - 1, amount)
	}
}

get_count : List(U64), U64 -> U64
get_count = |counts, index| match counts.get(index) {
	Ok(count) => count
	Err(_) => {
		crash "scratchcard count index out of bounds"
	}
}

example_input = 
	\\Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
	\\Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
	\\Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
	\\Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
	\\Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
	\\Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
