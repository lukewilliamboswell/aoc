app [main!] {
	pf: platform "../../basic-cli/platform/main.roc",
	parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/1.0.2/FrnJ4RGDKpQyoDyESNoBwFNviY4ZGbMVLnUjW9tvSRjk.tar.zst",
}

import pf.OsStr
import pf.Stdin
import pf.Stdout
import parser.Parser exposing [Parser]
import parser.String

Movement : [Forward(U64), Up(U64), Down(U64)]

Position : { horizontal : U64, depth : U64, aim : U64 }

main! : List(OsStr) => Try({}, _)
main! = |_args| {
	bytes = Stdin.read_to_end!()?
	input = Str.from_utf8(bytes) ? |err| InvalidUtf8(err)
	answer1 = part1(input) ? |err| SolverFailed(Str.inspect(err))
	answer2 = part2(input) ? |err| SolverFailed(Str.inspect(err))
	Stdout.line!("Part 1: ${answer1}")?
	Stdout.line!("Part 2: ${answer2}")?
	Ok({})
}

part1 : Str -> Try(Str, _)
part1 = |input| {
	movements = parse_input(input)?
	position = process_simple(movements)
	Ok("Final position is h:${position.horizontal.to_str()},d:${position.depth.to_str()}, result:${(position.horizontal * position.depth).to_str()}")
}

part2 : Str -> Try(Str, _)
part2 = |input| {
	movements = parse_input(input)?
	position = process_with_aim(movements)
	Ok("Final position is h:${position.horizontal.to_str()},d:${position.depth.to_str()}, result:${(position.horizontal * position.depth).to_str()}")
}

parse_input : Str -> Try(List(Movement), _)
parse_input = |content|
	String.parse_str(movement_parser.sep_by(String.codeunit('\n')), content.trim())

movement_parser : Parser(String.Utf8, Movement)
movement_parser = Parser.one_of([
	Parser.const(|amount| Forward(amount)).skip(String.string("forward ")).keep(String.digits),
	Parser.const(|amount| Down(amount)).skip(String.string("down ")).keep(String.digits),
	Parser.const(|amount| Up(amount)).skip(String.string("up ")).keep(String.digits),
])

process_simple : List(Movement) -> Position
process_simple = |movements|
	movements.fold(
		{ horizontal: 0, depth: 0, aim: 0 },
		|position, movement|
			match movement {
				Forward(amount) => { ..position, horizontal: position.horizontal + amount }
				Up(amount) => { ..position, depth: position.depth - amount }
				Down(amount) => { ..position, depth: position.depth + amount }
			},
	)

process_with_aim : List(Movement) -> Position
process_with_aim = |movements|
	movements.fold(
		{ horizontal: 0, depth: 0, aim: 0 },
		|position, movement|
			match movement {
				Forward(amount) => {
					..position,
					horizontal: position.horizontal + amount,
					depth: position.depth + amount * position.aim,
				}
				Up(amount) => { ..position, aim: position.aim - amount }
				Down(amount) => { ..position, aim: position.aim + amount }
			},
	)

example = 
	\\forward 5
	\\down 5
	\\forward 8
	\\up 3
	\\down 8
	\\forward 2

## Movement records parse from the strategy guide.
expect parse_input(example)? == [Forward(5), Down(5), Forward(8), Up(3), Down(8), Forward(2)]

## Part one applies movement directly to horizontal position and depth.
expect part1(example)? == "Final position is h:15,d:10, result:150"

## Part two applies forward movement using the current aim.
expect part2(example)? == "Final position is h:15,d:60, result:900"
