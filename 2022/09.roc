app [main!] {
	pf: platform "../../basic-cli/platform/main.roc",
}

import pf.OsStr
import pf.Stdin
import pf.Stdout

Direction : [Right, Left, Up, Down]

Move : { direction : Direction, count : U64 }

Position : { x : I64, y : I64 }

State : { head : Position, tails : List(Position), visits : Set(Position) }

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
	visits = process(input, 1)?
	Ok(visits.to_str())
}

part2 : Str -> Try(Str, _)
part2 = |input| {
	visits = process(input, 9)?
	Ok(visits.to_str())
}

process : Str, U64 -> Try(U64, _)
process = |input, tail_count| {
	moves = parse_moves(input.trim().split_on("\n"), [])?
	origin : Position
	origin = { x: 0, y: 0 }
	state = { head: origin, tails: List.repeat(origin, tail_count), visits: Set.single(origin) }
	final = apply_all_moves(state, moves)?
	Ok(final.visits.len())
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

parse_move : Str -> Try(Move, _)
parse_move = |line|
	match line.split_on(" ") {
		[direction_str, count_str] => {
			direction = match direction_str {
				"R" => Right
				"L" => Left
				"U" => Up
				"D" => Down
				_ => return Err(InvalidDirection(direction_str))
			}
			count = U64.from_str(count_str) ? |_| InvalidMove(line)
			Ok({ direction, count })
		}
		_ => Err(InvalidMove(line))
	}

apply_all_moves : State, List(Move) -> Try(State, _)
apply_all_moves = |state, moves|
	match moves {
		[] => Ok(state)
		[move, .. as rest] => {
			updated = apply_move(state, move.direction, move.count)?
			apply_all_moves(updated, rest)
		}
	}

apply_move : State, Direction, U64 -> Try(State, _)
apply_move = |state, direction, remaining| {
	if remaining == 0 {
		return Ok(state)
	}
	head = step_head(state.head, direction)
	tails = follow_all(state.tails, head, [])
	last = tails.last() ? |_| MissingTail
	updated = { head, tails, visits: state.visits.insert(last) }
	apply_move(updated, direction, remaining - 1)
}

step_head : Position, Direction -> Position
step_head = |head, direction|
	match direction {
		Right => { ..head, x: head.x + 1 }
		Left => { ..head, x: head.x - 1 }
		Up => { ..head, y: head.y + 1 }
		Down => { ..head, y: head.y - 1 }
	}

follow_all : List(Position), Position, List(Position) -> List(Position)
follow_all = |tails, leader, updated|
	match tails {
		[] => updated
		[tail, .. as rest] => {
			moved = follow(leader, tail)
			follow_all(rest, moved, updated.append(moved))
		}
	}

follow : Position, Position -> Position
follow = |leader, tail| {
	dx = leader.x - tail.x
	dy = leader.y - tail.y
	if dx >= -1 and dx <= 1 and dy >= -1 and dy <= 1 {
		tail
	} else {
		{ x: tail.x + sign(dx), y: tail.y + sign(dy) }
	}
}

sign : I64 -> I64
sign = |value|
	if value < 0 {
		-1
	} else if value > 0 {
		1
	} else {
		0
	}

example_input = 
	\\R 4
	\\U 4
	\\L 3
	\\D 1
	\\R 4
	\\D 1
	\\L 5
	\\R 2

bigger_example_input = 
	\\R 5
	\\U 8
	\\L 8
	\\D 3
	\\R 17
	\\D 10
	\\L 25
	\\U 20

## The short rope visits thirteen positions in the sample.
expect part1(example_input) == Ok("13")

## The long rope barely moves its tail in the small sample.
expect part2(example_input) == Ok("1")

## The larger sample exercises all knots in the long rope.
expect part2(bigger_example_input) == Ok("36")
