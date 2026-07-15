app [main!] {
	pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.21.0-rc3/Cv3oh6aPxqZxDeh1dgnXPjZnYsoqQMvEawa6TUc71sxJ.tar.zst",
	parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/1.0.2/FrnJ4RGDKpQyoDyESNoBwFNviY4ZGbMVLnUjW9tvSRjk.tar.zst",
}

import pf.OsStr
import pf.Stdin
import pf.Stdout
import parser.Parser exposing [Parser]
import parser.String

Cube : [Red(U64), Green(U64), Blue(U64)]

Game : { id : U64, reveals : List(List(Cube)) }

CubeSet : { red : U64, green : U64, blue : U64 }

main! : List(OsStr) => Try({}, _)
main! = |_| {
	input = Str.from_utf8(Stdin.read_to_end!()?) ? |err| InvalidUtf8(err)
	answer1 = part1(input) ? |err| SolverFailed(Str.inspect(err))
	answer2 = part2(input) ? |err| SolverFailed(Str.inspect(err))
	Stdout.line!("Part 1: ${answer1}")?
	Stdout.line!("Part 2: ${answer2}")?
	Ok({})
}

part1 : Str -> Try(Str, _)
part1 = |input| {
	games = String.parse_str(parse_game.sep_by(String.codeunit('\n')), input.trim())?
	total = games.keep_if(is_valid_game).map(|game| game.id).sum()
	Ok("The sum of the IDs is ${total.to_str()}")
}

part2 : Str -> Try(Str, _)
part2 = |input| {
	games = String.parse_str(parse_game.sep_by(String.codeunit('\n')), input.trim())?
	total = games.map(
		|game| {
			set = min_cube_set(game)
			set.red * set.green * set.blue
		},
	).sum()
	Ok("The sum of the power is ${total.to_str()}")
}

parse_cube : Parser(String.Utf8, Cube)
parse_cube = String.one_of([
	Parser.const(|count| Red(count)).keep(String.digits).skip(String.string(" red")),
	Parser.const(|count| Green(count)).keep(String.digits).skip(String.string(" green")),
	Parser.const(|count| Blue(count)).keep(String.digits).skip(String.string(" blue")),
])

## A cube parser reads its count and colour.
expect String.parse_str(parse_cube, "64 green") == Ok(Green(64))

parse_game : Parser(String.Utf8, Game)
parse_game = {
	id: Parser.const(|id| id).skip(String.string("Game ")).keep(String.digits).skip(String.string(": ")),
	reveals: parse_cube.sep_by(String.string(", ")).sep_by(String.string("; ")),
}.Parser

## A game parser groups cubes by reveal.
expect String.parse_str(parse_game, "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")
	== Ok({ id: 1, reveals: [[Blue(3), Red(4)], [Red(1), Green(2), Blue(6)], [Green(2)]] })

is_valid_game : Game -> Bool
is_valid_game = |game| game.reveals.all(|reveal| reveal.all(is_valid_cube))

is_valid_cube : Cube -> Bool
is_valid_cube = |cube| match cube {
	Red(count) => count <= 12
	Green(count) => count <= 13
	Blue(count) => count <= 14
}

## A reveal within the bag limits is valid.
expect is_valid_game({ id: 1, reveals: [[Blue(3), Red(4)], [Green(13)]] })

## A reveal over a bag limit is invalid.
expect !is_valid_game({ id: 1, reveals: [[Blue(15)]] })

min_cube_set : Game -> CubeSet
min_cube_set = |game|
	flatten(game.reveals).fold(
		{ red: 0, green: 0, blue: 0 },
		|set, cube| match cube {
			Red(count) => {
				..set,
				red: if count > set.red {
					count
				} else {
					set.red
				},
			}
			Green(count) => {
				..set,
				green: if count > set.green {
					count
				} else {
					set.green
				},
			}
			Blue(count) => {
				..set,
				blue: if count > set.blue {
					count
				} else {
					set.blue
				},
			}
		},
	)

flatten : List(List(a)) -> List(a)
flatten = |lists| lists.fold([], List.concat)

## The minimum set takes the maximum count of each colour.
expect min_cube_set({ id: 1, reveals: [[Blue(3), Red(4)], [Red(1), Green(2), Blue(6)]] })
	== { red: 4, green: 2, blue: 6 }

example_input = 
	\\Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
	\\Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
	\\Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
	\\Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
	\\Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green

## Part one sums the IDs of possible games.
expect part1(example_input) == Ok("The sum of the IDs is 8")

## Part two sums the powers of minimum cube sets.
expect part2(example_input) == Ok("The sum of the power is 2286")
