app [main!] { pf: platform "../../basic-cli/platform/main.roc" }

import pf.OsStr
import pf.Stdin
import pf.Stdout

Position : { r : I64, c : I64 }

Direction : [Up, Down, Left, Right]

Guard : { facing : Direction, pos : Position }

Map : { obstructions : Set(Position), guard : Guard, max_r : I64, max_c : I64 }

main! : List(OsStr) => Try({}, _)
main! = |_| {
	input = Str.from_utf8(Stdin.read_to_end!()?) ? |err| InvalidUtf8(err)
	Stdout.line!("Part 1: ${part1(input)}")?
	Stdout.line!("Part 2: ${part2(input)}")?
	Ok({})
}

part1 : Str -> Str
part1 = |input| {
	map = parse_map(input.trim())
	visited_positions(map.guard, map).len().to_str()
}

part2 : Str -> Str
part2 = |input| {
	map = parse_map(input.trim())
	candidates = visited_positions(map.guard, map).remove(map.guard.pos).to_list()
	candidates.count_if(
		|position|
			is_looping(map.guard, { ..map, obstructions: map.obstructions.insert(position) }, Set.empty()),
	).to_str()
}

## The sample guard visits 41 distinct positions.
expect part1(example_input) == "41"

## Six sample positions create a guard loop.
expect part2(example_input) == "6"

visited_positions : Guard, Map -> Set(Position)
visited_positions = |guard, map| visited_positions_help(guard, map, Set.single(guard.pos))

visited_positions_help : Guard, Map, Set(Position) -> Set(Position)
visited_positions_help = |guard, map, visited| {
	next = step_guard(guard, map.obstructions)
	if is_outside(next.pos, map) {
		visited
	} else {
		visited_positions_help(next, map, visited.insert(next.pos))
	}
}

is_looping : Guard, Map, Set(Guard) -> Bool
is_looping = |guard, map, seen| {
	if seen.contains(guard) {
		Bool.True
	} else {
		next = step_guard(guard, map.obstructions)
		if is_outside(next.pos, map) {
			Bool.False
		} else {
			is_looping(next, map, seen.insert(guard))
		}
	}
}

is_outside : Position, Map -> Bool
is_outside = |position, map|
	position.r < 0 or position.r > map.max_r or position.c < 0 or position.c > map.max_c

step_guard : Guard, Set(Position) -> Guard
step_guard = |{ facing, pos }, obstructions| {
	next_pos = match facing {
		Up => { r: pos.r - 1, c: pos.c }
		Down => { r: pos.r + 1, c: pos.c }
		Left => { r: pos.r, c: pos.c - 1 }
		Right => { r: pos.r, c: pos.c + 1 }
	}

	if obstructions.contains(next_pos) {
		step_guard({ facing: turn_right(facing), pos }, obstructions)
	} else {
		{ facing, pos: next_pos }
	}
}

turn_right : Direction -> Direction
turn_right = |direction| match direction {
	Up => Right
	Right => Down
	Down => Left
	Left => Up
}

## An unobstructed guard advances in its current direction.
expect step_guard({ facing: Up, pos: { r: 2, c: 2 } }, Set.empty()) == { facing: Up, pos: { r: 1, c: 2 } }

## An obstructed guard turns right before advancing.
expect {
	obstructions = Set.single({ r: 1, c: 2 })
	step_guard({ facing: Up, pos: { r: 2, c: 2 } }, obstructions) == { facing: Right, pos: { r: 2, c: 3 } }
}

parse_map : Str -> Map
parse_map = |input| {
	lines = input.split_on("\n")
	initial = {
		obstructions: Set.empty(),
		guard: { facing: Down, pos: { r: 0, c: 0 } },
	}

	parsed = lines.fold_with_index(
		initial,
		|state, line, row|
			line.to_utf8().fold_with_index(
				state,
				|inner, byte, column| {
					position = { r: row.to_i64_wrap(), c: column.to_i64_wrap() }
					match byte {
						'#' => { ..inner, obstructions: inner.obstructions.insert(position) }
						'^' => { ..inner, guard: { facing: Up, pos: position } }
						'>' => { ..inner, guard: { facing: Right, pos: position } }
						'v' => { ..inner, guard: { facing: Down, pos: position } }
						'<' => { ..inner, guard: { facing: Left, pos: position } }
						_ => inner
					}
				},
			),
	)

	max_c = match lines.get(0) {
		Ok(first) => (first.to_utf8().len() - 1).to_i64_wrap()
		Err(_) => -1
	}

	{
		obstructions: parsed.obstructions,
		guard: parsed.guard,
		max_r: (lines.len() - 1).to_i64_wrap(),
		max_c,
	}
}

## Map parsing locates the guard, obstacles, and bounds.
expect {
	{ obstructions, guard, max_r, max_c } = parse_map(example_input)
	expected_obstructions = Set.from_list([
		{ c: 4, r: 0 },
		{ c: 9, r: 1 },
		{ c: 2, r: 3 },
		{ c: 7, r: 4 },
		{ c: 1, r: 6 },
		{ c: 8, r: 7 },
		{ c: 0, r: 8 },
		{ c: 6, r: 9 },
	])
	actual = 
		\\guard matches: ${Str.inspect(guard == { facing: Up, pos: { r: 6, c: 4 } })}
		\\obstructions match: ${Str.inspect(obstructions == expected_obstructions)}
		\\bounds match: ${Str.inspect(max_r == 9 and max_c == 9)}
	expected = 
		\\guard matches: True
		\\obstructions match: True
		\\bounds match: True
	actual == expected
}

example_input = 
	\\....#.....
	\\.........#
	\\..........
	\\..#.......
	\\.......#..
	\\..........
	\\.#..^.....
	\\........#.
	\\#.........
	\\......#...
