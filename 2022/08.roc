app [main!] {
	pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.21.0-rc4/FvCh4vdqm3nBY6DWEfZ8RuGCVfjuMY43HA8KSNk9qVDn.tar.zst",
}

import pf.OsStr
import pf.Stdin
import pf.Stdout

Grid : { cells : List(U8), width : U64, height : U64 }

main! : List(OsStr) => Try({}, _)
main! = |_| {
	bytes = Stdin.read_to_end!()?
	input = Str.from_utf8(bytes) ? |err| InvalidUtf8(err)
	answer = part1(input)?
	Stdout.line!("Part 1: ${answer}")?
	Ok({})
}

part1 : Str -> Try(Str, _)
part1 = |input| {
	grid = parse_grid(input)?
	visible = count_visible(grid, 0, 0)?
	Ok(visible.to_str())
}

parse_grid : Str -> Try(Grid, _)
parse_grid = |input| {
	lines = input.trim().split_on("\n")
	first = lines.first() ? |_| EmptyGrid
	width = first.to_utf8().len()
	if width == 0 {
		return Err(EmptyGrid)
	}
	cells = parse_rows(lines, width, [])?
	Ok({ cells, width, height: lines.len() })
}

parse_rows : List(Str), U64, List(U8) -> Try(List(U8), _)
parse_rows = |lines, width, cells|
	match lines {
		[] => Ok(cells)
		[line, .. as rest] => {
			bytes = line.to_utf8()
			if bytes.len() != width {
				return Err(NonRectangularGrid)
			}
			heights = parse_heights(bytes, [])?
			parse_rows(rest, width, cells.concat(heights))
		}
	}

parse_heights : List(U8), List(U8) -> Try(List(U8), _)
parse_heights = |bytes, heights|
	match bytes {
		[] => Ok(heights)
		[byte, .. as rest] if byte >= '0' and byte <= '9' => parse_heights(rest, heights.append(byte - '0'))
		[byte, ..] => Err(InvalidHeight(byte))
	}

count_visible : Grid, U64, U64 -> Try(U64, _)
count_visible = |grid, index, total| {
	if index >= grid.cells.len() {
		return Ok(total)
	}
	visible = is_visible(grid, index)?
	count_visible(
		grid,
		index + 1,
		if visible {
			total + 1
		} else {
			total
		},
	)
}

is_visible : Grid, U64 -> Try(Bool, _)
is_visible = |grid, index| {
	row = index / grid.width
	col = index % grid.width
	if row == 0 or col == 0 or row + 1 == grid.height or col + 1 == grid.width {
		return Ok(True)
	}
	height = grid.cells.get(index)?
	left = clear_horizontal(grid, row, 0, col, height)?
	right = clear_horizontal(grid, row, col + 1, grid.width, height)?
	up = clear_vertical(grid, col, 0, row, height)?
	down = clear_vertical(grid, col, row + 1, grid.height, height)?
	Ok(left or right or up or down)
}

clear_horizontal : Grid, U64, U64, U64, U8 -> Try(Bool, _)
clear_horizontal = |grid, row, col, end, height| {
	if col >= end {
		return Ok(True)
	}
	other = grid.cells.get(row * grid.width + col)?
	if other >= height {
		Ok(False)
	} else {
		clear_horizontal(grid, row, col + 1, end, height)
	}
}

clear_vertical : Grid, U64, U64, U64, U8 -> Try(Bool, _)
clear_vertical = |grid, col, row, end, height| {
	if row >= end {
		return Ok(True)
	}
	other = grid.cells.get(row * grid.width + col)?
	if other >= height {
		Ok(False)
	} else {
		clear_vertical(grid, col, row + 1, end, height)
	}
}

example_input = 
	\\30373
	\\25512
	\\65332
	\\33549
	\\35390

## Twenty-one trees are visible from outside the sample grid.
expect part1(example_input) == Ok("21")

## A one-cell grid has one visible tree.
expect part1("7") == Ok("1")
