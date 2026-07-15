app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.21.0-rc3/Cv3oh6aPxqZxDeh1dgnXPjZnYsoqQMvEawa6TUc71sxJ.tar.zst" }

import pf.OsStr
import pf.Stdin
import pf.Stdout

main! : List(OsStr) => Try({}, _)
main! = |_| {
	input = Str.from_utf8(Stdin.read_to_end!()?) ? |err| InvalidUtf8(err)
	answer1 = part1(input) ? |err| SolverFailed(Str.inspect(err))
	answer2 = part2(input) ? |err| SolverFailed(Str.inspect(err))
	Stdout.line!("Part 1: ${answer1}")?
	Stdout.line!("Part 2: ${answer2}")?
	Ok({})
}

part1 : Str -> Try(Str, [NoDigit(Str)])
part1 = |input| {
	total = calibration_sum(input.trim().split_on("\n"), DigitsOnly, 0)?
	Ok("The sum of all of the calibration values ${total.to_str()}")
}

## Part one finds the first and last numeric characters on each line.
expect part1(example_input_part1) == Ok("The sum of all of the calibration values 142")

part2 : Str -> Try(Str, [NoDigit(Str)])
part2 = |input| {
	total = calibration_sum(input.trim().split_on("\n"), DigitsAndWords, 0)?
	Ok("The sum of all of the calibration values ${total.to_str()}")
}

calibration_sum : List(Str), [DigitsOnly, DigitsAndWords], U64 -> Try(U64, [NoDigit(Str)])
calibration_sum = |lines, mode, total| match lines {
	[] => Ok(total)
	[line, .. as rest] => {
		digits = match mode {
			DigitsOnly => line.to_utf8().keep_if(is_digit)
			DigitsAndWords => take_digits(line.to_utf8(), [])
		}
		value = calibration(line, digits)?
		calibration_sum(rest, mode, total + value)
	}
}

## Part two recognises overlapping digit words as well as numeric characters.
expect part2(example_input_part2) == Ok("The sum of all of the calibration values 281")

is_digit : U8 -> Bool
is_digit = |byte| byte >= '0' and byte <= '9'

calibration : Str, List(U8) -> Try(U64, [NoDigit(Str)])
calibration = |line, digits| match digits {
	[first, .., last] => Ok((first - '0').to_u64() * 10 + (last - '0').to_u64())
	[first] => Ok((first - '0').to_u64() * 11)
	[] => Err(NoDigit(line))
}

take_digits : List(U8), List(U8) -> List(U8)
take_digits = |rest, digits| match rest {
	[] => digits
	[first, .. as tail] if is_digit(first) => take_digits(tail, digits.append(first))
	['o', 'n', 'e', ..] => take_digits(rest.drop_first(1), digits.append('1'))
	['t', 'w', 'o', ..] => take_digits(rest.drop_first(1), digits.append('2'))
	['t', 'h', 'r', 'e', 'e', ..] => take_digits(rest.drop_first(1), digits.append('3'))
	['f', 'o', 'u', 'r', ..] => take_digits(rest.drop_first(1), digits.append('4'))
	['f', 'i', 'v', 'e', ..] => take_digits(rest.drop_first(1), digits.append('5'))
	['s', 'i', 'x', ..] => take_digits(rest.drop_first(1), digits.append('6'))
	['s', 'e', 'v', 'e', 'n', ..] => take_digits(rest.drop_first(1), digits.append('7'))
	['e', 'i', 'g', 'h', 't', ..] => take_digits(rest.drop_first(1), digits.append('8'))
	['n', 'i', 'n', 'e', ..] => take_digits(rest.drop_first(1), digits.append('9'))
	[_, .. as tail] => take_digits(tail, digits)
}

example_input_part1 = 
	\\1abc2
	\\pqr3stu8vwx
	\\a1b2c3d4e5f
	\\treb7uchet

example_input_part2 = 
	\\two1nine
	\\eightwothree
	\\abcone2threexyz
	\\xtwone3four
	\\4nineeightseven2
	\\zoneight234
	\\7pqrstsixteen
