app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.21.0-rc3/Cv3oh6aPxqZxDeh1dgnXPjZnYsoqQMvEawa6TUc71sxJ.tar.zst" }

import pf.OsStr
import pf.Stdin
import pf.Stdout

Shape : [Rock, Paper, Scissors]

Outcome : [Loss, Draw, Win]

Round : { opponent : Shape, guide : Outcome }

Choice : { opponent : Shape, choice : Shape }

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

part1 : Str -> Try(Str, [InvalidRound(Str)])
part1 = |input| {
	rounds = parse_input(input)?
	total = rounds.map(determine_choice).map(calculate_score).sum()
	Ok("The total score following guide ${total.to_str()}")
}

part2 : Str -> Try(Str, [InvalidRound(Str)])
part2 = |input| part1(input)

parse_input : Str -> Try(List(Round), [InvalidRound(Str)])
parse_input = |input|
	parse_lines(input.trim().split_on("\n"), [])

parse_lines : List(Str), List(Round) -> Try(List(Round), [InvalidRound(Str)])
parse_lines = |lines, rounds|
	match lines {
		[] => Ok(rounds)
		[first, .. as rest] => parse_lines(rest, rounds.append(parse_round(first)?))
	}

parse_round : Str -> Try(Round, [InvalidRound(Str)])
parse_round = |line| {
	if line == "A X" Ok({ opponent: Rock, guide: Loss })
	else if line == "A Y" Ok({ opponent: Rock, guide: Draw })
	else if line == "A Z" Ok({ opponent: Rock, guide: Win })
	else if line == "B X" Ok({ opponent: Paper, guide: Loss })
	else if line == "B Y" Ok({ opponent: Paper, guide: Draw })
	else if line == "B Z" Ok({ opponent: Paper, guide: Win })
	else if line == "C X" Ok({ opponent: Scissors, guide: Loss })
	else if line == "C Y" Ok({ opponent: Scissors, guide: Draw })
	else if line == "C Z" Ok({ opponent: Scissors, guide: Win })
	else Err(InvalidRound(line))
}

calculate_score : Choice -> U64
calculate_score = |choice| {
	base = match choice.choice {
		Rock => 1
		Paper => 2
		Scissors => 3
	}
	outcome = match determine_outcome(choice) {
		Loss => 0
		Draw => 3
		Win => 6
	}
	base + outcome
}

determine_outcome : Choice -> Outcome
determine_outcome = |{ opponent, choice }|
	match (opponent, choice) {
		(Rock, Rock) => Draw
		(Rock, Paper) => Win
		(Rock, Scissors) => Loss
		(Paper, Rock) => Loss
		(Paper, Paper) => Draw
		(Paper, Scissors) => Win
		(Scissors, Rock) => Win
		(Scissors, Paper) => Loss
		(Scissors, Scissors) => Draw
	}

determine_choice : Round -> Choice
determine_choice = |{ opponent, guide }| {
	rock = { opponent, choice: Rock }
	paper = { opponent, choice: Paper }
	if determine_outcome(rock) == guide rock else if determine_outcome(paper) == guide paper else { opponent, choice: Scissors }
}

example_input = 
	\\A Y
	\\B X
	\\C Z

## The guide parses into opponent shapes and desired outcomes.
expect parse_input(example_input)? == [
	{ opponent: Rock, guide: Draw },
	{ opponent: Paper, guide: Loss },
	{ opponent: Scissors, guide: Win },
]

## Following the sample guide produces a score of twelve.
expect part1(example_input) == Ok("The total score following guide 12")

## A chosen shape is scored using its shape and outcome values.
expect calculate_score({ opponent: Rock, choice: Paper }) == 8
