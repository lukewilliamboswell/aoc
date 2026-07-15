app [main!] {
	pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.21.0-rc3/Cv3oh6aPxqZxDeh1dgnXPjZnYsoqQMvEawa6TUc71sxJ.tar.zst",
}

import pf.OsStr
import pf.Stdin
import pf.Stdout

Position : { row : U64, col : U64 }

Grid : { cells : List(U8), width : U64, height : U64, start : Position, end : Position }

ParseState : { cells : List(U8), start : [Found(Position), Missing], end : [Found(Position), Missing] }

Search : { queue : List(Position), distances : Dict(Position, U64) }

main! : List(OsStr) => Try({}, _)
main! = |_| {
	bytes = Stdin.read_to_end!()?
	input = Str.from_utf8(bytes) ? |err| InvalidUtf8(err)
	answer1 = part1(input)?
	answer2 = part2(input)?
	Stdout.line!("Part 1: ${answer1}")?
	Stdout.line!("Part 2: ${answer2}")?
	Ok({})
}

part1 : Str -> Try(Str, _)
part1 = |input| {
	grid = parse_grid(input)?
	distances = distances_from_end(grid)?
	distance = distances.get(grid.start) ? |_| NoPathFromStart
	Ok(distance.to_str())
}

part2 : Str -> Try(Str, _)
part2 = |input| {
	grid = parse_grid(input)?
	distances = distances_from_end(grid)?
	candidates = low_point_distances(grid, distances, 0, [])?
	shortest = candidates.min() ? |_| NoReachableLowPoint
	Ok(shortest.to_str())
}

parse_grid : Str -> Try(Grid, _)
parse_grid = |input| {
	lines = input.trim().split_on("\n")
	first = lines.first() ? |_| EmptyGrid
	width = first.to_utf8().len()
	if width == 0 {
		return Err(EmptyGrid)
	}
	parsed = parse_rows(lines, width, 0, { cells: [], start: Missing, end: Missing })?
	start = match parsed.start {
		Found(position) => position
		Missing => return Err(MissingStart)
	}
	end = match parsed.end {
		Found(position) => position
		Missing => return Err(MissingEnd)
	}
	Ok({ cells: parsed.cells, width, height: lines.len(), start, end })
}

parse_rows : List(Str), U64, U64, ParseState -> Try(ParseState, _)
parse_rows = |lines, width, row, state|
	match lines {
		[] => Ok(state)
		[line, .. as rest] => {
			bytes = line.to_utf8()
			if bytes.len() != width {
				return Err(NonRectangularGrid)
			}
			updated = parse_row(bytes, row, 0, state)?
			parse_rows(rest, width, row + 1, updated)
		}
	}

parse_row : List(U8), U64, U64, ParseState -> Try(ParseState, _)
parse_row = |bytes, row, col, state|
	match bytes {
		[] => Ok(state)
		[byte, .. as rest] => {
			position = { row, col }
			updated = match byte {
				'S' => { ..state, cells: state.cells.append(0), start: Found(position) }
				'E' => { ..state, cells: state.cells.append(25), end: Found(position) }
				_ if byte >= 'a' and byte <= 'z' => { ..state, cells: state.cells.append(byte - 'a') }
				_ => return Err(InvalidHeight(byte))
			}
			parse_row(rest, row, col + 1, updated)
		}
	}

distances_from_end : Grid -> Try(Dict(Position, U64), _)
distances_from_end = |grid| {
	initial = { queue: [grid.end], distances: Dict.single(grid.end, 0) }
	breadth_first(grid, initial)
}

breadth_first : Grid, Search -> Try(Dict(Position, U64), _)
breadth_first = |grid, search|
	match search.queue {
		[] => Ok(search.distances)
		[current, .. as rest] => {
			distance = search.distances.get(current)?
			current_height = height_at(grid, current)?
			updated = visit_neighbors(grid, neighbors(grid, current), current_height, distance, { ..search, queue: rest })?
			breadth_first(grid, updated)
		}
	}

visit_neighbors : Grid, List(Position), U8, U64, Search -> Try(Search, _)
visit_neighbors = |grid, positions, current_height, distance, search|
	match positions {
		[] => Ok(search)
		[position, .. as rest] => {
			neighbor_height = height_at(grid, position)?
			updated = if !search.distances.contains(position) and neighbor_height + 1 >= current_height {
				{ queue: search.queue.append(position), distances: search.distances.insert(position, distance + 1) }
			} else {
				search
			}
			visit_neighbors(grid, rest, current_height, distance, updated)
		}
	}

neighbors : Grid, Position -> List(Position)
neighbors = |grid, { row, col }| {
	positions0 : List(Position)
	positions0 = []
	positions1 = if row > 0 {
		positions0.append({ row: row - 1, col })
	} else {
		positions0
	}
	positions2 = if row + 1 < grid.height {
		positions1.append({ row: row + 1, col })
	} else {
		positions1
	}
	positions3 = if col > 0 {
		positions2.append({ row, col: col - 1 })
	} else {
		positions2
	}
	if col + 1 < grid.width {
		positions3.append({ row, col: col + 1 })
	} else {
		positions3
	}
}

height_at : Grid, Position -> Try(U8, _)
height_at = |grid, { row, col }| grid.cells.get(row * grid.width + col)

low_point_distances : Grid, Dict(Position, U64), U64, List(U64) -> Try(List(U64), _)
low_point_distances = |grid, distances, index, candidates| {
	if index >= grid.cells.len() {
		return Ok(candidates)
	}
	height = grid.cells.get(index)?
	row = index / grid.width
	col = index % grid.width
	updated = if height == 0 {
		match distances.get({ row, col }) {
			Ok(distance) => candidates.append(distance)
			Err(_) => candidates
		}
	} else {
		candidates
	}
	low_point_distances(grid, distances, index + 1, updated)
}

example_input = 
	\\Sabqponm
	\\abcryxxl
	\\accszExk
	\\acctuvwj
	\\abdefghi

## The shortest sample route from S to E has thirty-one steps.
expect part1(example_input) == Ok("31")

## Starting from the best low point reduces the sample route to twenty-nine steps.
expect part2(example_input) == Ok("29")

## Reverse traversal exposes the two legal neighbors of the sample start.
expect {
	grid = parse_grid(example_input)?
	neighbors(grid, grid.start) == [{ row: 1, col: 0 }, { row: 0, col: 1 }]
}
