app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.21.0-rc4/FvCh4vdqm3nBY6DWEfZ8RuGCVfjuMY43HA8KSNk9qVDn.tar.zst" }

import pf.OsStr
import pf.Stdin
import pf.Stdout

Coord : { row : I64, col : I64 }

Direction : { row : I64, col : I64 }

Grid := List(List(U8)).{
	from_str : Str -> Grid
	from_str = |input| Grid.(input.trim().split_on("\n").map(Str.to_utf8))

	rows : Grid -> List(List(U8))
	rows = |Grid.(grid_rows)| grid_rows

	at : Grid, Coord -> [Found(U8), Missing]
	at = |Grid.(grid_rows), { row, col }| {
		if row < 0 or col < 0 {
			Missing
		} else {
			match grid_rows.get(row.to_u64_wrap()) {
				Ok(line) => match line.get(col.to_u64_wrap()) {
					Ok(byte) => Found(byte)
					Err(_) => Missing
				}
				Err(_) => Missing
			}
		}
	}
}

main! : List(OsStr) => Try({}, _)
main! = |_| {
	input = Str.from_utf8(Stdin.read_to_end!()?) ? |err| InvalidUtf8(err)
	Stdout.line!("Part 1: ${part1(input)}")?
	Stdout.line!("Part 2: ${part2(input)}")?
	Ok({})
}

part1 : Str -> Str
part1 = |input| {
	grid = Grid.from_str(input)
	var $total = 0.U64
	var $row = 0.I64
	for line in grid.rows() {
		var $col = 0.I64
		for _ in line {
			start = { row: $row, col: $col }
			for direction in directions if spells_xmas(grid, start, direction) {
				$total = $total + 1
			}
			$col = $col + 1
		}
		$row = $row + 1
	}
	$total.to_str()
}

## Part one finds XMAS in all eight directions.
expect part1(example_input) == "18"

part2 : Str -> Str
part2 = |input| {
	grid = Grid.from_str(input)
	var $total = 0.U64
	var $row = 0.I64
	for line in grid.rows() {
		var $col = 0.I64
		for _ in line {
			if is_x_mas(grid, { row: $row, col: $col }) {
				$total = $total + 1
			}
			$col = $col + 1
		}
		$row = $row + 1
	}
	$total.to_str()
}

## Part two finds crossed MAS words centred on A.
expect part2(example_input) == "9"

move : Coord, Direction, I64 -> Coord
move = |start, direction, steps| {
	row: start.row + direction.row * steps,
	col: start.col + direction.col * steps,
}

spells_xmas : Grid, Coord, Direction -> Bool
spells_xmas = |grid, start, direction|
	grid.at(start) == Found('X')
		and grid.at(move(start, direction, 1)) == Found('M')
			and grid.at(move(start, direction, 2)) == Found('A')
				and grid.at(move(start, direction, 3)) == Found('S')

is_mas_pair : [Found(U8), Missing], [Found(U8), Missing] -> Bool
is_mas_pair = |first, second|
	(first == Found('M') and second == Found('S'))
		or (first == Found('S') and second == Found('M'))

is_x_mas : Grid, Coord -> Bool
is_x_mas = |grid, middle| {
	if grid.at(middle) != Found('A') {
		Bool.False
	} else {
		is_mas_pair(grid.at(move(middle, { row: -1, col: -1 }, 1)), grid.at(move(middle, { row: 1, col: 1 }, 1)))
			and is_mas_pair(grid.at(move(middle, { row: -1, col: 1 }, 1)), grid.at(move(middle, { row: 1, col: -1 }, 1)))
	}
}

directions : List(Direction)
directions = [
	{ row: -1, col: -1 },
	{ row: -1, col: 0 },
	{ row: -1, col: 1 },
	{ row: 0, col: -1 },
	{ row: 0, col: 1 },
	{ row: 1, col: -1 },
	{ row: 1, col: 0 },
	{ row: 1, col: 1 },
]

example_input = 
	\\MMMSXXMASM
	\\MSAMXMSMSA
	\\AMXSXMAAMM
	\\MSAMASMSMX
	\\XMASAMXAMM
	\\XXAMMXXAMA
	\\SMSMSASXSS
	\\SAXAMASAAA
	\\MAMMMXMMMM
	\\MXMXAXMASX
