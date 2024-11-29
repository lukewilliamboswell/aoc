app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    aoc: "https://github.com/lukewilliamboswell/aoc-template/releases/download/0.2.0/tlS1ZkwSKSB87_3poSOXcwHyySe0WxWOWQbPmp7rxBw.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.9.0/w8YKp2YAgQt5REYk912HfKAHBjcXsrnvtjI0CBzoAT4.tar.br",
}

import pf.Stdin
import pf.Stdout
import pf.Utc
import aoc.AoC {
    stdin: Stdin.readToEnd,
    stdout: Stdout.write,
    time: \{} -> Utc.now {} |> Task.map Utc.toMillisSinceEpoch,
}
import parser.String exposing [Utf8, string, digits, parseStr, codeunit]
import parser.Parser exposing [Parser, sepBy]

main =
    AoC.solve {
        year: 2023,
        day: 5,
        title: "If You Give A Seed A Fertilizer",
        part1,
        part2: \_ -> Ok "Part 2 not implemented",
    }

Map : List { s : U64, d : U64, r : U64 }

Model : {
    seeds : List U64,
    seedRanges : List { s : U64, r : U64 },
    seedToSoil : Map,
    soilToFertilizer : Map,
    fertilizerToWater : Map,
    waterToLight : Map,
    lightToTemperature : Map,
    temperatureToHumidity : Map,
    humidityToLocation : Map,
}

map : Map -> (U64 -> U64)
map = \entries -> \from ->
    matches =
        List.keepOks entries \{ s, d, r } ->
            if from >= s && from < (s + r) then
                Ok (d + from - s)
            else
                Err {}

    expect List.len matches <= 1

    when matches is
        [] -> from
        [a, ..] -> a

part1 : Str -> Result Str _
part1 = \input ->
    model = try parseStr modelParser input
    lowest = try calculateLowest model

    Ok "The lowest location from initial seed numbers is $(Num.toStr lowest)"

expect part1 exampleInput == Ok "The lowest location from initial seed numbers is 35"

calculateLowest : Model -> Result U64 [ListWasEmpty]
calculateLowest = \model ->

    locations =
        model.seeds
        |> List.map (map model.seedToSoil)
        |> List.map (map model.soilToFertilizer)
        |> List.map (map model.fertilizerToWater)
        |> List.map (map model.waterToLight)
        |> List.map (map model.lightToTemperature)
        |> List.map (map model.temperatureToHumidity)
        |> List.map (map model.humidityToLocation)

    locations
    |> List.sortAsc
    |> List.first

seedParser : Parser Utf8 (List U64)
seedParser =
    { Parser.map2 <-
        _: string "seeds: ",
        seeds: sepBy digits (codeunit ' '),
    }
    |> Parser.map .seeds

expect parseStr seedParser "seeds: 79 14 55 13" == Ok [79, 14, 55, 13]

mapEntryParser : Parser Utf8 { s : U64, d : U64, r : U64 }
mapEntryParser =
    { Parser.map2 <-
        d: digits,
        _: codeunit ' ',
        s: digits,
        _: codeunit ' ',
        r: digits,
    }

mapParser : Str -> Parser Utf8 Map
mapParser = \heading ->
    { Parser.map2 <-
        _: string "$(heading) map:\n",
        map: sepBy mapEntryParser (codeunit '\n'),
    }
    |> Parser.map .map

expect
    input =
        """
        seed-to-soil map:
        50 98 2
        52 50 48
        """
    result = parseStr (mapParser "seed-to-soil") input
    result == Ok [{ s: 98, d: 50, r: 2 }, { s: 50, d: 52, r: 48 }]

modelParser : Parser Utf8 Model
modelParser =
    { Parser.map2 <-
        seedRanges: Parser.const [],
        seeds: seedParser,
        _: string "\n\n",
        seedToSoil: mapParser "seed-to-soil",
        _: string "\n\n",
        soilToFertilizer: mapParser "soil-to-fertilizer",
        _: string "\n\n",
        fertilizerToWater: mapParser "fertilizer-to-water",
        _: string "\n\n",
        waterToLight: mapParser "water-to-light",
        _: string "\n\n",
        lightToTemperature: mapParser "light-to-temperature",
        _: string "\n\n",
        temperatureToHumidity: mapParser "temperature-to-humidity",
        _: string "\n\n",
        humidityToLocation: mapParser "humidity-to-location",
    }
    |> Parser.map \model ->
        List.walk model.seeds Start \state, curr ->
            when state is
                Start -> One [] curr
                One ranges prev -> Two (List.append ranges { s: prev, r: curr })
                Two ranges -> One ranges curr
        |> \state ->
            when state is
                Two ranges -> { model & seedRanges: ranges }
                _ -> crash "expected even number of seeds to form ranges"

exampleInput =
    """
    seeds: 79 14 55 13

    seed-to-soil map:
    50 98 2
    52 50 48

    soil-to-fertilizer map:
    0 15 37
    37 52 2
    39 0 15

    fertilizer-to-water map:
    49 53 8
    0 11 42
    42 0 7
    57 7 4

    water-to-light map:
    88 18 7
    18 25 70

    light-to-temperature map:
    45 77 23
    81 45 19
    68 64 13

    temperature-to-humidity map:
    0 69 1
    1 0 69

    humidity-to-location map:
    60 56 37
    56 93 4
    """

expect
    result = parseStr modelParser exampleInput
    Result.isOk result
