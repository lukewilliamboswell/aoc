app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.21.0-rc3/Cv3oh6aPxqZxDeh1dgnXPjZnYsoqQMvEawa6TUc71sxJ.tar.zst" }

import pf.OsStr
import pf.Stdin
import pf.Stdout

Grid : List(List(U8))

Coord : { row : I64, col : I64 }

Direction : { row : I64, col : I64 }

main! : List(OsStr) => Try({}, _)
main! = |_| {
	input = Str.from_utf8(Stdin.read_to_end!()?) ? |err| InvalidUtf8(err)
	Stdout.line!("Part 1: ${part1(input)}")?
	Stdout.line!("Part 2: ${part2(input)}")?
	Ok({})
}

part1 : Str -> Str
part1 = |input| {
	grid = to_grid(input)
	total = all_coordinates(grid).map(
		|start|
			directions.count_if(|direction| spells_xmas(grid, start, direction)),
	).sum()
	total.to_str()
}

## Part one finds XMAS in all eight directions.
expect part1(example_input) == "18"

part2 : Str -> Str
part2 = |input| {
	grid = to_grid(input)
	all_coordinates(grid).count_if(|middle| is_x_mas(grid, middle)).to_str()
}

## Part two finds crossed MAS words centred on A.
expect part2(example_input) == "9"

to_grid : Str -> Grid
to_grid = |input| input.trim().split_on("\n").map(Str.to_utf8)

all_coordinates : Grid -> List(Coord)
all_coordinates = |grid|
	flatten(
		grid.map_with_index(
			|row, row_index|
				row.map_with_index(
					|_, col_index| {
						row: row_index.to_i64_wrap(),
						col: col_index.to_i64_wrap(),
					},
				),
		),
	)

flatten : List(List(a)) -> List(a)
flatten = |lists| lists.fold([], List.concat)

at : Grid, Coord -> [Found(U8), Missing]
at = |grid, { row, col }| {
	if row < 0 or col < 0 {
		Missing
	} else {
		match grid.get(row.to_u64_wrap()) {
			Ok(line) => match line.get(col.to_u64_wrap()) {
				Ok(byte) => Found(byte)
				Err(_) => Missing
			}
			Err(_) => Missing
		}
	}
}

move : Coord, Direction, I64 -> Coord
move = |start, direction, steps| {
	row: start.row + direction.row * steps,
	col: start.col + direction.col * steps,
}

spells_xmas : Grid, Coord, Direction -> Bool
spells_xmas = |grid, start, direction|
	at(grid, start) == Found('X')
		and at(grid, move(start, direction, 1)) == Found('M')
			and at(grid, move(start, direction, 2)) == Found('A')
				and at(grid, move(start, direction, 3)) == Found('S')

is_mas_pair : [Found(U8), Missing], [Found(U8), Missing] -> Bool
is_mas_pair = |first, second|
	(first == Found('M') and second == Found('S'))
		or (first == Found('S') and second == Found('M'))

is_x_mas : Grid, Coord -> Bool
is_x_mas = |grid, middle| {
	if at(grid, middle) != Found('A') {
		Bool.False
	} else {
		is_mas_pair(at(grid, move(middle, { row: -1, col: -1 }, 1)), at(grid, move(middle, { row: 1, col: 1 }, 1)))
			and is_mas_pair(at(grid, move(middle, { row: -1, col: 1 }, 1)), at(grid, move(middle, { row: 1, col: -1 }, 1)))
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
