app [main!] {
	pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.21.0-rc3/Cv3oh6aPxqZxDeh1dgnXPjZnYsoqQMvEawa6TUc71sxJ.tar.zst",
}

import pf.OsStr
import pf.Stdin
import pf.Stdout

Move : { count : U64, from : U64, to : U64 }

Stacks : Dict(U64, List(Str))

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
	{ stacks, moves } = parse(input)?
	moved = apply_moves_one_at_a_time(stacks, moves)?
	top_crates(moved)
}

part2 : Str -> Try(Str, _)
part2 = |input| {
	{ stacks, moves } = parse(input)?
	moved = apply_moves_together(stacks, moves)?
	top_crates(moved)
}

parse : Str -> Try({ stacks : Stacks, moves : List(Move) }, _)
parse = |input| {
	match input.trim_end().split_on("\n\n") {
		[diagram, instructions] => {
			stacks = parse_stacks(diagram)?
			moves = parse_moves(instructions.split_on("\n"), [])?
			Ok({ stacks, moves })
		}
		_ => Err(InvalidInputSections)
	}
}

parse_stacks : Str -> Try(Stacks, _)
parse_stacks = |diagram| {
	lines = diagram.split_on("\n")
	index_line = lines.last() ? |_| MissingStackNumbers
	stack_count = index_line.split_on(" ").keep_if(|part| !part.is_empty()).len()
	initial = init_stacks(1, stack_count, Dict.empty())
	rows = lines.drop_last(1).rev()
	add_stack_rows(initial, rows, stack_count)
}

add_stack_rows : Stacks, List(Str), U64 -> Try(Stacks, _)
add_stack_rows = |stacks, rows, count|
	match rows {
		[] => Ok(stacks)
		[row, .. as rest] => {
			updated = add_stack_row(stacks, row, count)?
			add_stack_rows(updated, rest, count)
		}
	}

init_stacks : U64, U64, Stacks -> Stacks
init_stacks = |index, count, stacks|
	if index > count {
		stacks
	} else {
		init_stacks(index + 1, count, stacks.insert(index, []))
	}

add_stack_row : Stacks, Str, U64 -> Try(Stacks, _)
add_stack_row = |stacks, row, count| add_stack_cell(stacks, row.to_utf8(), 1, count)

add_stack_cell : Stacks, List(U8), U64, U64 -> Try(Stacks, _)
add_stack_cell = |stacks, bytes, stack_index, count| {
	if stack_index > count {
		return Ok(stacks)
	}

	byte_index = 1 + (stack_index - 1) * 4
	updated = match bytes.get(byte_index) {
		Ok(byte) if byte != ' ' => {
			crate = Str.from_utf8([byte]) ? |err| InvalidCrate(err)
			stack = stacks.get(stack_index) ? |_| MissingStack(stack_index)
			stacks.insert(stack_index, stack.append(crate))
		}
		_ => stacks
	}
	add_stack_cell(updated, bytes, stack_index + 1, count)
}

parse_move : Str -> Try(Move, _)
parse_move = |line|
	match line.split_on(" ") {
		["move", count, "from", from, "to", to] => Ok({
			count: U64.from_str(count) ? |_| InvalidMove(line),
			from: U64.from_str(from) ? |_| InvalidMove(line),
			to: U64.from_str(to) ? |_| InvalidMove(line),
		})
		_ => Err(InvalidMove(line))
	}

parse_moves : List(Str), List(Move) -> Try(List(Move), _)
parse_moves = |lines, moves|
	match lines {
		[] => Ok(moves)
		[line, .. as rest] => {
			move = parse_move(line)?
			parse_moves(rest, moves.append(move))
		}
	}

apply_moves_one_at_a_time : Stacks, List(Move) -> Try(Stacks, _)
apply_moves_one_at_a_time = |stacks, moves|
	match moves {
		[] => Ok(stacks)
		[move, .. as rest] => {
			updated = apply_move_one_at_a_time(stacks, move)?
			apply_moves_one_at_a_time(updated, rest)
		}
	}

apply_moves_together : Stacks, List(Move) -> Try(Stacks, _)
apply_moves_together = |stacks, moves|
	match moves {
		[] => Ok(stacks)
		[move, .. as rest] => {
			updated = apply_move_together(stacks, move)?
			apply_moves_together(updated, rest)
		}
	}

apply_move_one_at_a_time : Stacks, Move -> Try(Stacks, _)
apply_move_one_at_a_time = |stacks, { count, from, to }| {
	from_stack = stacks.get(from) ? |_| MissingStack(from)
	to_stack = stacks.get(to) ? |_| MissingStack(to)
	if count > from_stack.len() {
		return Err(NotEnoughCrates({ stack: from, requested: count }))
	}

	ordered = from_stack.take_last(count).rev()
	Ok(stacks.insert(from, from_stack.drop_last(count)).insert(to, to_stack.concat(ordered)))
}

apply_move_together : Stacks, Move -> Try(Stacks, _)
apply_move_together = |stacks, { count, from, to }| {
	from_stack = stacks.get(from) ? |_| MissingStack(from)
	to_stack = stacks.get(to) ? |_| MissingStack(to)
	if count > from_stack.len() {
		return Err(NotEnoughCrates({ stack: from, requested: count }))
	}

	ordered = from_stack.take_last(count)
	Ok(stacks.insert(from, from_stack.drop_last(count)).insert(to, to_stack.concat(ordered)))
}

top_crates : Stacks -> Try(Str, _)
top_crates = |stacks| {
	count = stacks.len()
	top_crates_help(stacks, 1, count, [])
}

top_crates_help : Stacks, U64, U64, List(Str) -> Try(Str, _)
top_crates_help = |stacks, index, count, crates| {
	if index > count {
		return Ok(Str.join_with(crates, ""))
	}
	stack = stacks.get(index) ? |_| MissingStack(index)
	crate = stack.last() ? |_| EmptyStack(index)
	top_crates_help(stacks, index + 1, count, crates.append(crate))
}

example_input = 
	\\    [D]
	\\[N] [C]
	\\[Z] [M] [P]
	\\ 1   2   3
	\\
	\\move 1 from 2 to 1
	\\move 3 from 1 to 3
	\\move 2 from 2 to 1
	\\move 1 from 1 to 2

## Move instructions capture the count and stack indexes.
expect parse_move("move 2 from 3 to 1") == Ok({ count: 2, from: 3, to: 1 })

## The sample diagram and instruction list parse successfully.
expect parse(example_input).is_ok()

## The sample solutions produce the expected top crates.
expect {
	part1(example_input)? == "CMZ" and part2(example_input)? == "MCD"
}

## The CrateMover 9000 reverses crates moved one at a time.
expect {
	stacks = Dict.empty().insert(1, ["A", "B"]).insert(2, ["C"])
	updated = apply_move_one_at_a_time(stacks, { count: 2, from: 1, to: 2 })?
	updated.get(2)? == ["C", "B", "A"]
}

## The CrateMover 9001 preserves crates moved together.
expect {
	stacks = Dict.empty().insert(1, ["A", "B"]).insert(2, ["C"])
	updated = apply_move_together(stacks, { count: 2, from: 1, to: 2 })?
	updated.get(2)? == ["C", "A", "B"]
}
