app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.16.0/O00IPk-Krg_diNS2dVWlI0ZQP794Vctxzv0ha96mK0E.tar.br",
    aoc: "https://github.com/lukewilliamboswell/aoc-template/releases/download/0.1.0/DcTQw_U67F22cX7pgx93AcHz_ShvHRaFIFjcijF3nz0.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.8.0/PCkJq9IGyIpMfwuW-9hjfXd6x-bHb1_OZdacogpBcPM.tar.br",
}

import pf.Stdin
import pf.Stdout
import pf.Utc
import aoc.AoC {
    stdin: Stdin.readToEnd,
    stdout: Stdout.write,
    time: \{} -> Utc.now {} |> Task.map Utc.toMillisSinceEpoch,
}
import parser.String exposing [Utf8, digits, parseStr, codeunit]
import parser.Parser exposing [Parser, sepBy, const, keep, skip]

main =
    AoC.solve {
        year: 2023,
        day: 5,
        title: "If You Give A Seed A Fertilizer",
        part1,
        part2,
    }

Map : List { s : U64, d : U64, r : U64 }

Model : {
    seeds : List U64,
    #seedRanges : List (List U64),
    #seedToSoil : Map,
    #soilToFertilizer : Map,
    #fertilizerToWater : Map,
    #waterToLight : Map,
    #lightToTemperature : Map,
    #temperatureToHumidity : Map,
    #humidityToLocation : Map,
}

#map : Map -> (U64 -> U64)
#map = \entries -> \from ->
#        matches =
#            List.keepOks entries \{ s, d, r } ->

#                # check all the ranges without branching
#                if from >= s && from < (s + r) then
#                    Ok (d + from - s)
#                else
#                    Err {}

#        expect List.len matches <= 1

#        when matches is
#            [] -> from
#            [a, ..] -> a

part1 : Str -> Result Str _
part1 = \_input ->
    Err TODO
#    model = parse input |> Result.mapErr? ParseErr

#    lowest = calculateLowest? model UseSeeds

#    Ok "The lowest location from initial seed numbers is $(Num.toStr lowest)"

#expect part1 exampleInput == Ok "The lowest location from initial seed numbers is 35"

part2 : Str -> Result Str _
part2 = \_input ->
    Err TODO
#    model = parse input |> Result.mapErr? ParseErr2

#    lowest = calculateLowest? model UseSeedRanges

#    Ok "The lowest location from seed ranges is $(Num.toStr lowest)"


#expect part2 exampleInput == Ok "The lowest location from seed ranges is 46"

#calculateLowest : Model, [UseSeeds, UseSeedRanges] -> Result U64 _
#calculateLowest = \model, select ->
#    seeds =
#        when select is
#            UseSeeds -> model.seeds
#            UseSeedRanges -> List.join model.seedRanges

#    locations =
#        seeds
#        |> List.map (map model.seedToSoil)
#        |> List.map (map model.soilToFertilizer)
#        |> List.map (map model.fertilizerToWater)
#        |> List.map (map model.waterToLight)
#        |> List.map (map model.lightToTemperature)
#        |> List.map (map model.temperatureToHumidity)
#        |> List.map (map model.humidityToLocation)

#    locations
#    |> List.sortAsc
#    |> List.first

#entryParser : Parser (List U8) { s : U64, d : U64, r : U64 }
#entryParser =
#    const (\d -> \s -> \r -> { d, s, r })
#    |> keep digits
#    |> skip (codeunit ' ')
#    |> keep digits
#    |> skip (codeunit ' ')
#    |> keep digits

#expect parseStr entryParser "50 98 2" == Ok { s: 98, d: 50, r: 2 }

#parse : Str -> Result Model [Err Str]
#parse = \input ->

#parseSeedRange : Parser (List U8) (List U64)
#parseSeedRange =
#    const (\start -> \length -> List.range {start : At start, end : Before (start + length)})
#    |> keep digits
#    |> skip (codeunit ' ')
#    |> keep digits

#modelParser : Parser Utf8 Model
#modelParser =
#    { Parser.map2 <-
#        seeds: string "seeds: " |> andThen (sepBy digits (codeunit ' ')), # "seeds: "
#        seedRanges: parseStr (sepBy parseSeedRange (codeunit ' ')), # "seeds: "
#        #seedToSoil: parseStr (sepBy entryParser (codeunit '\n')), # "seed-to-soil map:\n"
#        #soilToFertilizer: parseStr (sepBy entryParser (codeunit '\n')), # "soil-to-fertilizer map:\n"
#        #fertilizerToWater: parseStr (sepBy entryParser (codeunit '\n')), # "fertilizer-to-water map:\n"
#        #waterToLight: parseStr (sepBy entryParser (codeunit '\n')), # "water-to-light map:\n"
#        #lightToTemperature: parseStr (sepBy entryParser (codeunit '\n')), # "light-to-temperature map:\n"
#        #temperatureToHumidity: parseStr (sepBy entryParser (codeunit '\n')), # "temperature-to-humidity map:\n"
#        #humidityToLocation: parseStr (sepBy entryParser (codeunit '\n')), # "humidity-to-location map:\n"
#    }

#exampleParsed : Model
#exampleParsed = {
#    seeds: [79, 14, 55, 13],
#    #fertilizerToWater: [
#    #    { d: 49, r: 8, s: 53 },
#    #    { d: 0, r: 42, s: 11 },
#    #    { d: 42, r: 7, s: 0 },
#    #    { d: 57, r: 4, s: 7 },
#    #],
#    #humidityToLocation: [
#    #    { d: 60, r: 37, s: 56 },
#    #    { d: 56, r: 4, s: 93 },
#    #],
#    #lightToTemperature: [
#    #    { d: 45, r: 23, s: 77 },
#    #    { d: 81, r: 19, s: 45 },
#    #    { d: 68, r: 13, s: 64 },
#    #],
#    #seedToSoil: [
#    #    { d: 50, r: 2, s: 98 },
#    #    { d: 52, r: 48, s: 50 },
#    #],
#    #soilToFertilizer: [
#    #    { d: 0, r: 37, s: 15 },
#    #    { d: 37, r: 2, s: 52 },
#    #    { d: 39, r: 15, s: 0 },
#    #],
#    #temperatureToHumidity: [
#    #    { d: 0, r: 1, s: 69 },
#    #    { d: 1, r: 69, s: 0 },
#    #],
#    #waterToLight: [
#    #    { d: 88, r: 7, s: 18 },
#    #    { d: 18, r: 70, s: 25 },
#    #],
#}

##expect
##    result = parseStr modelParser exampleInput
##    result == Ok exampleParsed

##exampleInput =
##    """
##    seeds: 79 14 55 13

##    seed-to-soil map:
##    50 98 2
##    52 50 48

##    soil-to-fertilizer map:
##    0 15 37
##    37 52 2
##    39 0 15

##    fertilizer-to-water map:
##    49 53 8
##    0 11 42
##    42 0 7
##    57 7 4

##    water-to-light map:
##    88 18 7
##    18 25 70

##    light-to-temperature map:
##    45 77 23
##    81 45 19
##    68 64 13

##    temperature-to-humidity map:
##    0 69 1
##    1 0 69

##    humidity-to-location map:
##    60 56 37
##    56 93 4
##    """
