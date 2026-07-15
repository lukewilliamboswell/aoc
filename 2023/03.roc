app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.21.0-rc3/Cv3oh6aPxqZxDeh1dgnXPjZnYsoqQMvEawa6TUc71sxJ.tar.zst" }

import pf.OsStr
import pf.Stdin
import pf.Stdout

Coord : { row : U64, col : U64 }

PartNumber : { row : U64, first_col : U64, last_col : U64, value : U64 }

main! : List(OsStr) => Try({}, _)
main! = |_| {
	input = Str.from_utf8(Stdin.read_to_end!()?) ? |err| InvalidUtf8(err)
	Stdout.line!("Part 1: ${part1(input)}")?
	Stdout.line!("Part 2: ${part2(input)}")?
	Ok({})
}

part1 : Str -> Str
part1 = |input| {
	rows = input.trim().split_on("\n").map(Str.to_utf8)
	numbers = find_numbers(rows)
	symbols = find_symbols(rows, AnySymbol)
	numbers.keep_if(|number| symbols.any(|symbol| is_adjacent(number, symbol))).map(|number| number.value).sum().to_str()
}

## Part one sums numbers adjacent to any symbol.
expect part1(example_input) == "4361"

part2 : Str -> Str
part2 = |input| {
	rows = input.trim().split_on("\n").map(Str.to_utf8)
	numbers = find_numbers(rows)
	gears = find_symbols(rows, GearsOnly)
	gears.map(
		|gear| match numbers.keep_if(|number| is_adjacent(number, gear)) {
			[first, second] => first.value * second.value
			_ => 0
		},
	).sum().to_str()
}

## Part two sums ratios for gears adjacent to exactly two numbers.
expect part2(example_input) == "467835"

is_digit : U8 -> Bool
is_digit = |byte| byte >= '0' and byte <= '9'

find_numbers : List(List(U8)) -> List(PartNumber)
find_numbers = |rows|
	flatten(rows.map_with_index(|row, row_index| find_numbers_in_row(row, row_index, 0, [])))

find_numbers_in_row : List(U8), U64, U64, List(PartNumber) -> List(PartNumber)
find_numbers_in_row = |remaining, row, col, result| match remaining {
	[] => result
	[first, ..] if is_digit(first) => {
		parsed = consume_digits(remaining, col, 0)
		number = { row, first_col: col, last_col: parsed.next_col - 1, value: parsed.value }
		find_numbers_in_row(parsed.remaining, row, parsed.next_col, result.append(number))
	}
	[_, .. as rest] => find_numbers_in_row(rest, row, col + 1, result)
}

consume_digits : List(U8), U64, U64 -> { remaining : List(U8), next_col : U64, value : U64 }
consume_digits = |remaining, col, value| match remaining {
	[first, .. as rest] if is_digit(first) => consume_digits(rest, col + 1, value * 10 + (first - '0').to_u64())
	_ => { remaining, next_col: col, value }
}

find_symbols : List(List(U8)), [AnySymbol, GearsOnly] -> List(Coord)
find_symbols = |rows, mode|
	flatten(
		rows.map_with_index(
			|row, row_index|
				row.map_with_index(|byte, col_index| { byte, coord: { row: row_index, col: col_index } })
					.keep_if(
						|item| match mode {
							AnySymbol => item.byte != '.' and !is_digit(item.byte)
							GearsOnly => item.byte == '*'
						},
					)
					.map(|item| item.coord),
		),
	)

flatten : List(List(a)) -> List(a)
flatten = |lists| lists.fold([], List.concat)

is_adjacent : PartNumber, Coord -> Bool
is_adjacent = |number, coord|
	number.row.abs_diff(coord.row) <= 1
		and coord.col + 1 >= number.first_col
			and coord.col <= number.last_col + 1

## A number is adjacent to symbols touching any digit, including diagonally.
expect is_adjacent({ row: 2, first_col: 2, last_col: 3, value: 35 }, { row: 1, col: 3 })

example_input = 
	\\467..114..
	\\...*......
	\\..35..633.
	\\......#...
	\\617*......
	\\.....+.58.
	\\..592.....
	\\......755.
	\\...$.*....
	\\.664.598..
