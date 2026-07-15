app [main!] {
	pf: platform "../../basic-cli/platform/main.roc",
	parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/1.0.2/FrnJ4RGDKpQyoDyESNoBwFNviY4ZGbMVLnUjW9tvSRjk.tar.zst",
}

import pf.OsStr
import pf.Stdin
import pf.Stdout
import parser.Parser exposing [Parser]
import parser.String

History : List(I64)

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
	histories = String.parse_str(parse_history.sep_by(String.codeunit('\n')), input.trim())?
	total = histories.map(|history| predict(Last, history)).sum()
	Ok("The sum of the LAST extrapolated values ${total.to_str()}")
}

## Part one extrapolates the next value of every history.
expect part1(example_input) == Ok("The sum of the LAST extrapolated values 114")

part2 : Str -> Try(Str, _)
part2 = |input| {
	histories = String.parse_str(parse_history.sep_by(String.codeunit('\n')), input.trim())?
	total = histories.map(|history| predict(First, history)).sum()
	Ok("The sum of the FIRST extrapolated values ${total.to_str()}")
}

## Part two extrapolates the preceding value of every history.
expect part2(example_input) == Ok("The sum of the FIRST extrapolated values 2")

parse_number : Parser(String.Utf8, I64)
parse_number = String.one_of([
	String.digits.map(U64.to_i64_wrap),
	Parser.const(|number| -number.to_i64_wrap()).skip(String.codeunit('-')).keep(String.digits),
])

parse_history : Parser(String.Utf8, History)
parse_history = parse_number.sep_by(String.codeunit(' '))

## History parsing supports positive and negative numbers.
expect String.parse_str(parse_history, "0 3 6 9 12 -15") == Ok([0, 3, 6, 9, 12, -15])

predict : [First, Last], History -> I64
predict = |direction, history| {
	if history.all(|number| number == 0) {
		0
	} else {
		next_history = differences(history)
		match direction {
			First => first_or_crash(history) - predict(direction, next_history)
			Last => last_or_crash(history) + predict(direction, next_history)
		}
	}
}

first_or_crash : History -> I64
first_or_crash = |history| match history.first() {
	Ok(value) => value
	Err(_) => {
		crash "expected a non-empty history"
	}
}

last_or_crash : History -> I64
last_or_crash = |history| match history.last() {
	Ok(value) => value
	Err(_) => {
		crash "expected a non-empty history"
	}
}

## Prediction extends an arithmetic history at the end.
expect predict(Last, [0, 3, 6, 9, 12, 15]) == 18

## Prediction extends a history at the beginning.
expect predict(First, [10, 13, 16, 21, 30, 45]) == 5

differences : History -> History
differences = |history| match history {
	[first, .. as rest] => differences_help(rest, first, [])
	[] => []
}

differences_help : History, I64, History -> History
differences_help = |remaining, previous, result| match remaining {
	[] => result
	[current, .. as rest] => differences_help(rest, current, result.append(current - previous))
}

## Differences subtract each value from its successor.
expect differences([1, 3, 6, 10, 15, 21]) == [2, 3, 4, 5, 6]

example_input = 
	\\0 3 6 9 12 15
	\\1 3 6 10 15 21
	\\10 13 16 21 30 45
