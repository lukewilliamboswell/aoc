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
	reports = String.parse_str(parse_report.sep_by(String.codeunit('\n')), input.trim())?
	count = reports.count_if(is_safe)
	Ok("${count.to_str()} many reports are safe!")
}

part2 : Str -> Try(Str, _)
part2 = |input| {
	reports = String.parse_str(parse_report.sep_by(String.codeunit('\n')), input.trim())?
	count = reports.count_if(is_safe_tolerant)
	Ok("${count.to_str()} many reports are safe!")
}

## Two sample reports are safe without dampening.
expect part1(example_input) == Ok("2 many reports are safe!")

## Four sample reports are safe with dampening.
expect part2(example_input) == Ok("4 many reports are safe!")

example_input = 
	\\7 6 4 2 1
	\\1 2 7 8 9
	\\9 7 6 2 1
	\\1 3 2 4 5
	\\8 6 4 4 1
	\\1 3 6 7 9

parse_report : Parser(String.Utf8, List(U64))
parse_report = String.digits.sep_by(String.codeunit(' '))

## A report row parses its levels.
expect String.parse_str(parse_report, "7 6 4 2 1") == Ok([7, 6, 4, 2, 1])

is_increasing_safely : List(U64) -> Bool
is_increasing_safely = |levels| {
	match levels {
		[first, .. as rest] => {
			result = rest.fold_until(
				Ok(first),
				|maybe_prev, curr| {
					prev = unwrap(maybe_prev)
					if curr > prev and safe_difference(curr, prev) {
						Continue(Ok(curr))
					} else {
						Break(Err({}))
					}
				},
			)
			match result {
				Ok(_) => Bool.True
				Err(_) => Bool.False
			}
		}
		_ => {
			crash "expected at least one level"
		}
	}
}

is_decreasing_safely : List(U64) -> Bool
is_decreasing_safely = |levels| {
	match levels {
		[first, .. as rest] => {
			result = rest.fold_until(
				Ok(first),
				|maybe_prev, curr| {
					prev = unwrap(maybe_prev)
					if curr < prev and safe_difference(curr, prev) {
						Continue(Ok(curr))
					} else {
						Break(Err({}))
					}
				},
			)
			match result {
				Ok(_) => Bool.True
				Err(_) => Bool.False
			}
		}
		_ => {
			crash "expected at least one level"
		}
	}
}

safe_difference : U64, U64 -> Bool
safe_difference = |curr, prev| {
	diff = curr.abs_diff(prev)
	diff >= 1 and diff <= 3
}

is_safe : List(U64) -> Bool
is_safe = |levels| is_increasing_safely(levels) or is_decreasing_safely(levels)

is_increasing_safely_tolerant : List(U64) -> Bool
is_increasing_safely_tolerant = |levels|
	levels.map_with_index(|_, idx| levels.drop_at(idx)).any(is_increasing_safely)

is_decreasing_safely_tolerant : List(U64) -> Bool
is_decreasing_safely_tolerant = |levels|
	levels.map_with_index(|_, idx| levels.drop_at(idx)).any(is_decreasing_safely)

is_safe_tolerant : List(U64) -> Bool
is_safe_tolerant = |levels| is_increasing_safely_tolerant(levels) or is_decreasing_safely_tolerant(levels)

## A steadily increasing report is safe.
expect is_increasing_safely([1, 2, 3, 4, 5])

## A one-level change is safe.
expect safe_difference(1, 2)

## A three-level change is safe.
expect safe_difference(1, 4)

## Equal levels are unsafe.
expect !safe_difference(1, 1)

## A four-level change is unsafe.
expect !safe_difference(1, 5)

## The decreasing sample report is safe.
expect is_safe([7, 6, 4, 2, 1])

## A large increasing jump is unsafe.
expect !is_safe([1, 2, 7, 8, 9])

## A large decreasing jump is unsafe.
expect !is_safe([9, 7, 6, 2, 1])

## A direction change is unsafe without dampening.
expect !is_safe([1, 3, 2, 4, 5])

## Equal adjacent levels are unsafe without dampening.
expect !is_safe([8, 6, 4, 4, 1])

## A gradual increasing report is safe.
expect is_safe([1, 3, 6, 7, 9])

## An already-safe report remains safe with dampening.
expect is_safe_tolerant([7, 6, 4, 2, 1])

## Dampening cannot repair multiple large increases.
expect !is_safe_tolerant([1, 2, 7, 8, 9])

## Dampening cannot repair multiple large decreases.
expect !is_safe_tolerant([9, 7, 6, 2, 1])

## Dampening repairs a single direction change.
expect is_safe_tolerant([1, 3, 2, 4, 5])

## Dampening repairs one equal-level pair.
expect is_safe_tolerant([8, 6, 4, 4, 1])

## A gradual report is tolerant-safe.
expect is_safe_tolerant([1, 3, 6, 7, 9])

## Dampening repairs a repeated level in a longer report.
expect is_safe_tolerant([1, 3, 3, 6, 7, 9])

unwrap : [Ok(a), Err(err)] -> a
unwrap = |result| {
	match result {
		Ok(value) => value
		Err(_) => {
			crash "expected Ok"
		}
	}
}
