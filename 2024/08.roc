app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.21.0-rc4/FvCh4vdqm3nBY6DWEfZ8RuGCVfjuMY43HA8KSNk9qVDn.tar.zst" }

import pf.OsStr
import pf.Stdin
import pf.Stdout

Position : { r : I64, c : I64 }

main! : List(OsStr) => Try({}, _)
main! = |_| {
	input = Str.from_utf8(Stdin.read_to_end!()?) ? |err| InvalidUtf8(err)
	Stdout.line!("Part 1: ${part1(input)}")?
	Ok({})
}

part1 : Str -> Str
part1 = |input| {
	{ max_r, max_c, antennas } = parse_antennas(input.trim())
	in_map = |{ r, c }| r >= 0 and r <= max_r and c >= 0 and c <= max_c

	unique_locations = antennas.fold(
		Set.empty(),
		|unique, _, locations| add_antinodes(locations, unique),
	)

	unique_locations.keep_if(in_map).len().to_str()
}

## The sample contains fourteen in-bounds antinodes.
expect part1(example_input) == "14"

anti_nodes : Position, Position -> { first : Position, second : Position }
anti_nodes = |first, second| {
	r_diff = (first.r - second.r).abs()
	c_diff = (first.c - second.c).abs()

	rows = if first.r < second.r {
		{ first: first.r - r_diff, second: second.r + r_diff }
	} else {
		{ first: first.r + r_diff, second: second.r - r_diff }
	}
	cols = if first.c < second.c {
		{ first: first.c - c_diff, second: second.c + c_diff }
	} else {
		{ first: first.c + c_diff, second: second.c - c_diff }
	}

	{
		first: { r: rows.first, c: cols.first },
		second: { r: rows.second, c: cols.second },
	}
}

## Antinodes extend the row and column difference in both directions.
expect anti_nodes({ r: 3, c: 4 }, { r: 5, c: 5 }) == { first: { c: 3, r: 1 }, second: { c: 6, r: 7 } }

## Antinodes may extend beyond the map bounds.
expect anti_nodes({ r: 3, c: 4 }, { r: 4, c: 8 }) == { first: { c: 0, r: 2 }, second: { c: 12, r: 5 } }

## Antinode direction follows both input coordinates.
expect anti_nodes({ r: 1, c: 8 }, { r: 2, c: 5 }) == { first: { r: 0, c: 11 }, second: { r: 3, c: 2 } }

add_antinodes : List(Position), Set(Position) -> Set(Position)
add_antinodes = |nodes, antinodes| {
	match nodes {
		[] => antinodes
		[first, .. as rest] => {
			updated = rest.fold(
				antinodes,
				|set, second| {
					pair = anti_nodes(first, second)
					set.insert(pair.first).insert(pair.second)
				},
			)
			add_antinodes(rest, updated)
		}
	}
}

## Pair traversal inserts both antinodes without materialising the pairs.
expect {
	actual = add_antinodes([{ c: 6, r: 5 }, { c: 8, r: 8 }, { c: 9, r: 9 }], Set.empty())
	expected = Set.from_list([
		{ r: 2, c: 4 },
		{ r: 11, c: 10 },
		{ r: 1, c: 3 },
		{ r: 13, c: 12 },
		{ r: 7, c: 7 },
		{ r: 10, c: 10 },
	])
	actual == expected
}

parse_antennas : Str -> { max_r : I64, max_c : I64, antennas : Dict(U8, List(Position)) }
parse_antennas = |input| {
	lines = input.split_on("\n")
	max_r = (lines.len() - 1).to_i64_wrap()
	max_c = match lines.get(0) {
		Ok(first) => (first.to_utf8().len() - 1).to_i64_wrap()
		Err(_) => -1
	}

	antennas = lines.fold_with_index(
		Dict.empty(),
		|line_dict, line, row| {
			line.to_utf8().fold_with_index(
				line_dict,
				|dict, byte, column| {
					if byte == '.' {
						dict
					} else {
						position = { r: row.to_i64_wrap(), c: column.to_i64_wrap() }
						dict.update(
							byte,
							|state| match state {
								Err(Missing) => Ok([position])
								Ok(list) => Ok(list.append(position))
							},
						)
					}
				},
			)
		},
	)

	{ max_r, max_c, antennas }
}

## Antenna parsing groups coordinates by frequency.
expect {
	actual = parse_antennas(example_input)
	expected = {
		max_r: 11,
		max_c: 11,
		antennas: Dict.from_list([
			('0', [{ c: 8, r: 1 }, { c: 5, r: 2 }, { c: 7, r: 3 }, { c: 4, r: 4 }]),
			('A', [{ c: 6, r: 5 }, { c: 8, r: 8 }, { c: 9, r: 9 }]),
		]),
	}
	actual == expected
}

example_input = 
	\\............
	\\........0...
	\\.....0......
	\\.......0....
	\\....0.......
	\\......A.....
	\\............
	\\............
	\\........A...
	\\.........A..
	\\............
	\\............
