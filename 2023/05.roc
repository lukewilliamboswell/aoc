app [main!] {
	pf: platform "../../basic-cli/platform/main.roc",
	parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/1.0.2/FrnJ4RGDKpQyoDyESNoBwFNviY4ZGbMVLnUjW9tvSRjk.tar.zst",
}

import pf.OsStr
import pf.Stdin
import pf.Stdout
import parser.Parser exposing [Parser]
import parser.String

MapEntry : { source : U64, destination : U64, length : U64 }

Map : List(MapEntry)

Model : { seeds : List(U64), maps : List(Map) }

main! : List(OsStr) => Try({}, _)
main! = |_| {
	input = Str.from_utf8(Stdin.read_to_end!()?) ? |err| InvalidUtf8(err)
	answer = part1(input) ? |err| SolverFailed(Str.inspect(err))
	Stdout.line!("Part 1: ${answer}")?
	Ok({})
}

part1 : Str -> Try(Str, [InputParseFailed(Str), NoSeeds])
part1 = |input| {
	model = String.parse_str(parse_model, input.trim()) ? |err| InputParseFailed(Str.inspect(err))
	lowest = calculate_lowest(model) ? |_| NoSeeds
	Ok("The lowest location from initial seed numbers is ${lowest.to_str()}")
}

## Part one maps each example seed to its location and chooses the lowest.
expect part1(example_input) == Ok("The lowest location from initial seed numbers is 35")

map_value : Map, U64 -> U64
map_value = |entries, value| match entries.keep_if(|entry| value >= entry.source and value < entry.source + entry.length) {
	[] => value
	[entry] => entry.destination + value - entry.source
	_ => {
		crash "overlapping map entries"
	}
}

calculate_lowest : Model -> Try(U64, [ListWasEmpty])
calculate_lowest = |model| {
	locations = model.seeds.map(|seed| model.maps.fold(seed, |value, entries| map_value(entries, value)))
	match locations.min() {
		Ok(lowest) => Ok(lowest)
		Err(_) => Err(ListWasEmpty)
	}
}

parse_seeds : Parser(String.Utf8, List(U64))
parse_seeds = Parser.const(|seeds| seeds)
	.skip(String.string("seeds: "))
	.keep(String.digits.sep_by(String.codeunit(' ')))

## Seed parsing reads every initial seed number.
expect String.parse_str(parse_seeds, "seeds: 79 14 55 13") == Ok([79, 14, 55, 13])

parse_map_entry : Parser(String.Utf8, MapEntry)
parse_map_entry = Parser.const(|destination| |source| |length| { source, destination, length })
	.keep(String.digits)
	.skip(String.codeunit(' '))
	.keep(String.digits)
	.skip(String.codeunit(' '))
	.keep(String.digits)

parse_map : Str -> Parser(String.Utf8, Map)
parse_map = |heading|
	Parser.const(|entries| entries)
		.skip(String.string("${heading} map:\n"))
		.keep(parse_map_entry.sep_by(String.codeunit('\n')))

## A map parser preserves source, destination, and range length.
expect String.parse_str(parse_map("seed-to-soil"), "seed-to-soil map:\n50 98 2\n52 50 48")
	== Ok([
		{ source: 98, destination: 50, length: 2 },
		{ source: 50, destination: 52, length: 48 },
	])

parse_model : Parser(String.Utf8, Model)
parse_model = 
	Parser.const(
		|seeds| |seed_to_soil| |soil_to_fertilizer| |fertilizer_to_water| |water_to_light| |light_to_temperature| |temperature_to_humidity| |humidity_to_location| {
			seeds,
			maps: [seed_to_soil, soil_to_fertilizer, fertilizer_to_water, water_to_light, light_to_temperature, temperature_to_humidity, humidity_to_location],
		},
	)
		.keep(parse_seeds)
		.skip(String.string("\n\n"))
		.keep(parse_map("seed-to-soil"))
		.skip(String.string("\n\n"))
		.keep(parse_map("soil-to-fertilizer"))
		.skip(String.string("\n\n"))
		.keep(parse_map("fertilizer-to-water"))
		.skip(String.string("\n\n"))
		.keep(parse_map("water-to-light"))
		.skip(String.string("\n\n"))
		.keep(parse_map("light-to-temperature"))
		.skip(String.string("\n\n"))
		.keep(parse_map("temperature-to-humidity"))
		.skip(String.string("\n\n"))
		.keep(parse_map("humidity-to-location"))

example_input = 
	\\seeds: 79 14 55 13
	\\
	\\seed-to-soil map:
	\\50 98 2
	\\52 50 48
	\\
	\\soil-to-fertilizer map:
	\\0 15 37
	\\37 52 2
	\\39 0 15
	\\
	\\fertilizer-to-water map:
	\\49 53 8
	\\0 11 42
	\\42 0 7
	\\57 7 4
	\\
	\\water-to-light map:
	\\88 18 7
	\\18 25 70
	\\
	\\light-to-temperature map:
	\\45 77 23
	\\81 45 19
	\\68 64 13
	\\
	\\temperature-to-humidity map:
	\\0 69 1
	\\1 0 69
	\\
	\\humidity-to-location map:
	\\60 56 37
	\\56 93 4
