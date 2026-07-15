app [main!] {
	pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.21.0-rc3/Cv3oh6aPxqZxDeh1dgnXPjZnYsoqQMvEawa6TUc71sxJ.tar.zst",
	parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/1.0.2/FrnJ4RGDKpQyoDyESNoBwFNviY4ZGbMVLnUjW9tvSRjk.tar.zst",
}

import pf.OsStr
import pf.Stdin
import pf.Stdout
import parser.Parser exposing [Parser]
import parser.String

Calibration : { target : U64, inputs : List(U64) }

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
part1 = |input| solve(input, Bool.False)

part2 : Str -> Try(Str, _)
part2 = |input| solve(input, Bool.True)

solve : Str, Bool -> Try(Str, _)
solve = |input, allow_concat| {
	calibrations = String.parse_str(parse_calibration.sep_by(String.codeunit('\n')), input.trim())?
	total = calibrations.keep_if(|calibration| calibration_is_valid(calibration, allow_concat)).map(|item| item.target).sum()
	Ok(total.to_str())
}

## Addition and multiplication validate the Part 1 sample calibrations.
expect part1(example_input) == Ok("3749")

## Concatenation validates the additional Part 2 calibrations.
expect part2(example_input) == Ok("11387")

calibration_is_valid : Calibration, Bool -> Bool
calibration_is_valid = |{ target, inputs }, allow_concat| {
	match inputs {
		[] => Bool.False
		[first, .. as rest] => can_reach(target, first, rest, allow_concat)
	}
}

can_reach : U64, U64, List(U64), Bool -> Bool
can_reach = |target, current, remaining, allow_concat| {
	match remaining {
		[] => current == target
		[next, .. as rest] => {
			add_matches = can_reach(target, current + next, rest, allow_concat)
			multiply_matches = can_reach(target, current * next, rest, allow_concat)
			concat_matches = if allow_concat {
				match concat_digits(current, next) {
					Ok(value) => can_reach(target, value, rest, allow_concat)
					Err(_) => Bool.False
				}
			} else {
				Bool.False
			}

			add_matches or multiply_matches or concat_matches
		}
	}
}

## Addition can satisfy a two-input calibration.
expect calibration_is_valid({ target: 190, inputs: [10, 180] }, Bool.False)

## Multiplication can satisfy a two-input calibration.
expect calibration_is_valid({ target: 190, inputs: [10, 19] }, Bool.False)

## Concatenation is disabled for Part 1.
expect !calibration_is_valid({ target: 156, inputs: [15, 6] }, Bool.False)

## Concatenation is enabled for Part 2.
expect calibration_is_valid({ target: 156, inputs: [15, 6] }, Bool.True)

concat_digits : U64, U64 -> Try(U64, _)
concat_digits = |left, right| U64.from_str("${left.to_str()}${right.to_str()}")

## Decimal concatenation joins the right value after the left value.
expect concat_digits(15, 6) == Ok(156)

parse_calibration : Parser(String.Utf8, Calibration)
parse_calibration = 
	Parser.const(|target| |inputs| { target, inputs })
		.keep(String.digits)
		.skip(String.string(": "))
		.keep(String.digits.sep_by(String.codeunit(' ')))

## Calibration parsing separates the target from its inputs.
expect String.parse_str(parse_calibration, "190: 10 19") == Ok({ target: 190, inputs: [10, 19] })

## Calibration parsing accepts longer input lists.
expect String.parse_str(parse_calibration, "21037: 9 7 18 13") == Ok({ target: 21037, inputs: [9, 7, 18, 13] })

example_input = 
	\\190: 10 19
	\\3267: 81 40 27
	\\83: 17 5
	\\156: 15 6
	\\7290: 6 8 6 15
	\\161011: 16 10 13
	\\192: 17 8 14
	\\21037: 9 7 18 13
	\\292: 11 6 16 20
