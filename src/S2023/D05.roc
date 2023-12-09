interface S2023.D05
    exposes [solution]
    imports [
        AoC,
        Parser.Core.{ Parser, const, keep, skip, sepBy },
        Parser.String.{ parseStr, digits, codeunit },
        "2023-05.txt" as puzzleInput : Str,
    ]

solution : AoC.Solution
solution = { year: 2023, day: 5, title: "If You Give A Seed A Fertilizer", part1, part2, puzzleInput }

Map : List { s : Nat, d : Nat, r : Nat }

Model : {
    seeds : List Nat,
    seedRanges : List (List Nat),
    seedToSoil : Map,
    soilToFertilizer : Map,
    fertilizerToWater : Map,
    waterToLight : Map,
    lightToTemperature : Map,
    temperatureToHumidity : Map,
    humidityToLocation : Map,
}

map : Map -> (Nat -> Nat)
map = \entries -> \from ->
        matches =
            List.keepOks entries \{ s, d, r } ->

                # check all the ranges without branching
                if from >= s && from < (s + r) then
                    Ok (d + from - s)
                else
                    Err {}

        expect List.len matches <= 1

        when matches is
            [] -> from
            [a, ..] -> a

part1 : Str -> Result Str [NotImplemented, Error Str]
part1 = \input ->

    model <- parse input |> Result.mapErr Error |> Result.try

    lowest <- 
        calculateLowest model UseSeeds
        |> Result.mapErr \_ -> Error "expected at least one location"
        |> Result.try

    Ok "The lowest location from initial seed numbers is \(Num.toStr lowest)"

expect part1 exampleInput == Ok "The lowest location from initial seed numbers is 35"

part2 : Str -> Result Str [NotImplemented, Error Str]
part2 = \input -> 

    model <- parse input |> Result.mapErr Error |> Result.try

    lowest <- 
        calculateLowest model UseSeedRanges
        |> Result.mapErr \_ -> Error "expected at least one location"
        |> Result.try

    Ok "The lowest location from seed ranges is \(Num.toStr lowest)"


expect part2 exampleInput == Ok "The lowest location from seed ranges is 46"

calculateLowest : Model, [UseSeeds, UseSeedRanges] -> Result Nat _
calculateLowest = \model, select ->
    seeds = 
        when select is 
            UseSeeds -> model.seeds
            UseSeedRanges -> List.join model.seedRanges

    locations =
        seeds
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

entryParser : Parser (List U8) { s : Nat, d : Nat, r : Nat }
entryParser =
    const (\d -> \s -> \r -> { d, s, r })
    |> keep digits
    |> skip (codeunit ' ')
    |> keep digits
    |> skip (codeunit ' ')
    |> keep digits

expect parseStr entryParser "50 98 2" == Ok { s: 98, d: 50, r: 2 }

parse : Str -> Result Model Str
parse = \input ->

    parseSeedRange : Parser (List U8) (List Nat)
    parseSeedRange = 
        const (\start -> \length -> List.range {start : At start, end : Before (start + length)}) 
        |> keep digits 
        |> skip (codeunit ' ')
        |> keep digits

    when input |> Str.split "\n\n" is
        [rawSeeds, s2s, s2f, f2w, w2l, l2t, t2h, h2l] ->
            seeds <-
                parseStr (sepBy digits (codeunit ' ')) (Str.replaceFirst rawSeeds "seeds: " "")
                |> Result.mapErr \_ -> "unable to parse seeds"
                |> Result.try

            seedRanges <- 
                parseStr (sepBy parseSeedRange (codeunit ' ')) (Str.replaceFirst rawSeeds "seeds: " "")
                |> Result.mapErr \_ -> "unable to parse seed ranges"
                |> Result.try

            seedToSoil <-
                parseStr (sepBy entryParser (codeunit '\n')) (Str.replaceFirst s2s "seed-to-soil map:\n" "")
                |> Result.mapErr \_ -> "unable to parse seed-to-soil map"
                |> Result.try

            soilToFertilizer <-
                parseStr (sepBy entryParser (codeunit '\n')) (Str.replaceFirst s2f "soil-to-fertilizer map:\n" "")
                |> Result.mapErr \_ -> "unable to parse soil-to-fertilizer map"
                |> Result.try

            fertilizerToWater <-
                parseStr (sepBy entryParser (codeunit '\n')) (Str.replaceFirst f2w "fertilizer-to-water map:\n" "")
                |> Result.mapErr \_ -> "unable to parse fertilizer-to-water map"
                |> Result.try

            waterToLight <-
                parseStr (sepBy entryParser (codeunit '\n')) (Str.replaceFirst w2l "water-to-light map:\n" "")
                |> Result.mapErr \_ -> "unable to parse water-to-light map"
                |> Result.try

            lightToTemperature <-
                parseStr (sepBy entryParser (codeunit '\n')) (Str.replaceFirst l2t "light-to-temperature map:\n" "")
                |> Result.mapErr \_ -> "unable to parse light-to-temperature map"
                |> Result.try

            temperatureToHumidity <-
                parseStr (sepBy entryParser (codeunit '\n')) (Str.replaceFirst t2h "temperature-to-humidity map:\n" "")
                |> Result.mapErr \_ -> "unable to parse temperature-to-humidity map"
                |> Result.try

            humidityToLocation <-
                parseStr (sepBy entryParser (codeunit '\n')) (Str.replaceFirst h2l "humidity-to-location map:\n" "")
                |> Result.mapErr \_ -> "unable to parse humidity-to-location map"
                |> Result.try

            Ok {
                seeds,
                seedRanges,
                seedToSoil,
                soilToFertilizer,
                fertilizerToWater,
                waterToLight,
                lightToTemperature,
                temperatureToHumidity,
                humidityToLocation,
            }

        _ -> Err "unexpected input"

# exampleParsed : Model
# exampleParsed = {
#     seeds: [79, 14, 55, 13],
#     fertilizerToWater: [
#         { d: 49, r: 8, s: 53 },
#         { d: 0, r: 42, s: 11 },
#         { d: 42, r: 7, s: 0 },
#         { d: 57, r: 4, s: 7 },
#     ],
#     humidityToLocation: [
#         { d: 60, r: 37, s: 56 },
#         { d: 56, r: 4, s: 93 },
#     ],
#     lightToTemperature: [
#         { d: 45, r: 23, s: 77 },
#         { d: 81, r: 19, s: 45 },
#         { d: 68, r: 13, s: 64 },
#     ],
#     seedToSoil: [
#         { d: 50, r: 2, s: 98 },
#         { d: 52, r: 48, s: 50 },
#     ],
#     soilToFertilizer: [
#         { d: 0, r: 37, s: 15 },
#         { d: 37, r: 2, s: 52 },
#         { d: 39, r: 15, s: 0 },
#     ],
#     temperatureToHumidity: [
#         { d: 0, r: 1, s: 69 },
#         { d: 1, r: 69, s: 0 },
#     ],
#     waterToLight: [
#         { d: 88, r: 7, s: 18 },
#         { d: 18, r: 70, s: 25 },
#     ],
# }

# expect parse exampleInput == Ok exampleParsed

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
