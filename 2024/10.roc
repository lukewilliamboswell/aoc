app [main!] { pf: platform "../../basic-cli/platform/main.roc" }

import pf.OsStr
import pf.Stdin
import pf.Stdout

Position : { r : I64, c : I64 }

HeightMap : Dict(Position, U8)

Direction : [Up, Down, Left, Right]

main! : List(OsStr) => Try({}, _)
main! = |_| {
	input = Str.from_utf8(Stdin.read_to_end!()?) ? |err| InvalidUtf8(err)
	Stdout.line!("Part 1: ${part1(input)}")?
	Stdout.line!("Part 2: ${part2(input)}")?
	Ok({})
}

part1 : Str -> Str
part1 = |input| {
	height_map = parse_map(input.trim())
	trail_heads(height_map).map(|head| score_trail(head, height_map)).sum().to_str()
}

part2 : Str -> Str
part2 = |input| {
	height_map = parse_map(input.trim())
	trail_heads(height_map).map(|head| rate_trail(head, height_map)).sum().to_str()
}

trail_heads : HeightMap -> List(Position)
trail_heads = |height_map|
	height_map.fold(
		[],
		|heads, position, height|
			if height == '0' {
				heads.append(position)
			} else {
				heads
			},
	)

## The first sample has two reachable peaks.
expect part1(example_map_1) == "2"

## The second sample has four reachable peaks.
expect part1(example_map_2) == "4"

## The third sample has three reachable peaks.
expect part1(example_map_3) == "3"

## The full sample has a score of 36.
expect part1(example_map_4) == "36"

## The rating sample contains three distinct trails.
expect part2(example_map_5) == "3"

rate_trail : Position, HeightMap -> U64
rate_trail = |position, height_map| {
	height = height_map.get(position) ?? '0'
	if height == '9' {
		1
	} else {
		next_steps(position, height, height_map).map(|next| rate_trail(next.position, height_map)).sum()
	}
}

score_trail : Position, HeightMap -> U64
score_trail = |position, height_map|
	reachable_peaks(position, height_map).len()

reachable_peaks : Position, HeightMap -> Set(Position)
reachable_peaks = |position, height_map| {
	height = height_map.get(position) ?? '0'
	if height == '9' {
		Set.single(position)
	} else {
		next_steps(position, height, height_map).fold(
			Set.empty(),
			|peaks, next|
				peaks.union(reachable_peaks(next.position, height_map)),
		)
	}
}

next_steps : Position, U8, HeightMap -> List({ position : Position, height : U8 })
next_steps = |position, height, height_map|
	[Up, Down, Left, Right].fold(
		[],
		|steps, direction|
			match step(position, height, direction, height_map) {
				Ok(next) => steps.append(next)
				Err(_) => steps
			},
	)

step : Position, U8, Direction, HeightMap -> Try({ position : Position, height : U8 }, [InvalidStep])
step = |position, height, direction, height_map| {
	next_position = match direction {
		Up => { r: position.r - 1, c: position.c }
		Down => { r: position.r + 1, c: position.c }
		Left => { r: position.r, c: position.c - 1 }
		Right => { r: position.r, c: position.c + 1 }
	}

	match height_map.get(next_position) {
		Ok(next_height) if next_height == height + 1 => Ok({ position: next_position, height: next_height })
		_ => Err(InvalidStep)
	}
}

## A downhill neighbor is a valid step from zero to one.
expect {
	height_map = parse_map(example_map_1)
	step({ c: 3, r: 0 }, '0', Down, height_map) == Ok({ position: { c: 3, r: 1 }, height: '1' })
}

## Moving back to a lower height is invalid.
expect {
	height_map = parse_map(example_map_1)
	step({ c: 3, r: 1 }, '1', Up, height_map) == Err(InvalidStep)
}

## A left neighbor exactly one level higher is valid.
expect {
	height_map = parse_map(example_map_1)
	step({ c: 3, r: 3 }, '3', Left, height_map) == Ok({ position: { c: 2, r: 3 }, height: '4' })
}

## A right neighbor exactly one level higher is valid.
expect {
	height_map = parse_map(example_map_1)
	step({ c: 3, r: 3 }, '3', Right, height_map) == Ok({ position: { c: 4, r: 3 }, height: '4' })
}

parse_map : Str -> HeightMap
parse_map = |input|
	input.split_on("\n").fold_with_index(
		Dict.empty(),
		|dict, row, r|
			row.to_utf8().fold_with_index(
				dict,
				|inner, byte, c|
					if byte == '.' {
						inner
					} else {
						inner.insert({ r: r.to_i64_wrap(), c: c.to_i64_wrap() }, byte)
					},
			),
	)

## Map parsing records every numeric position and ignores dots.
expect {
	actual = parse_map(example_map_1)
	expected = Dict.from_list([
		({ c: 3, r: 0 }, '0'),
		({ c: 3, r: 1 }, '1'),
		({ c: 3, r: 2 }, '2'),
		({ c: 0, r: 3 }, '6'),
		({ c: 1, r: 3 }, '5'),
		({ c: 2, r: 3 }, '4'),
		({ c: 3, r: 3 }, '3'),
		({ c: 4, r: 3 }, '4'),
		({ c: 5, r: 3 }, '5'),
		({ c: 6, r: 3 }, '6'),
		({ c: 0, r: 4 }, '7'),
		({ c: 6, r: 4 }, '7'),
		({ c: 0, r: 5 }, '8'),
		({ c: 6, r: 5 }, '8'),
		({ c: 0, r: 6 }, '9'),
		({ c: 6, r: 6 }, '9'),
	])
	actual == expected
}

example_map_1 = 
	\\...0...
	\\...1...
	\\...2...
	\\6543456
	\\7.....7
	\\8.....8
	\\9.....9

example_map_2 = 
	\\..90..9
	\\...1.98
	\\...2..7
	\\6543456
	\\765.987
	\\876....
	\\987....

example_map_3 = 
	\\10..9..
	\\2...8..
	\\3...7..
	\\4567654
	\\...8..3
	\\...9..2
	\\.....01

example_map_4 = 
	\\89010123
	\\78121874
	\\87430965
	\\96549874
	\\45678903
	\\32019012
	\\01329801
	\\10456732

example_map_5 = 
	\\.....0.
	\\..4321.
	\\..5..2.
	\\..6543.
	\\..7..4.
	\\..8765.
	\\..9....
