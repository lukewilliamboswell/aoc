app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.15.0/SlwdbJ-3GR7uBWQo6zlmYWNYOxnvo8r6YABXD-45UOw.tar.br",
    aoc: "https://github.com/lukewilliamboswell/aoc-template/releases/download/0.1.0/DcTQw_U67F22cX7pgx93AcHz_ShvHRaFIFjcijF3nz0.tar.br",
}

import pf.Stdin
import pf.Stdout
import pf.Utc
import aoc.AoC {
    stdin: Stdin.bytes,
    stdout: Stdout.write,
    time: \{} -> Utc.now {} |> Task.map Utc.toMillisSinceEpoch,
}

main =
    AoC.solve {
        year: 2022,
        day: 1,
        title: "Calorie Counting",
        part1,
        part2,
    }

exampleInput =
    """
    1000
    2000
    3000

    4000

    5000
    6000

    7000
    8000
    9000

    10000
    """

part1 : Str -> Result Str [NotImplemented, Error Str]
part1 = \input ->

    elfCalories = parse input

    sortedCals =
        elfCalories
        |> List.map List.sum
        |> List.sortDesc

    sortedCals
    |> List.first
    |> Result.mapErr \ListWasEmpty -> Error "list was empty, nothin in inventory"
    |> Result.map \highestCals ->
        "The Elf with the highest calories has $(Num.toStr highestCals) kCal"

expect part1 exampleInput == Ok "The Elf with the highest calories has 24000 kCal"

part2 : Str -> Result Str [NotImplemented, Error Str]
part2 = \input ->

    elfCalories = parse input

    sortedCals =
        elfCalories
        |> List.map List.sum
        |> List.sortDesc

    sumOfTopThree =
        (
            when sortedCals is
                [first, second, third, ..] -> Ok (first + second + third)
                _ -> Err (Error "should have more than three elves")
        )?

    Ok "Total kCal the Elves are carrying is $(Num.toStr sumOfTopThree)"

expect part2 exampleInput == Ok "Total kCal the Elves are carrying is 45000"

parse : Str -> List (List U64)
parse = \str ->
    str
    |> Str.split "\n\n"
    |> List.map \inventory ->
        inventory
        |> Str.split "\n"
        |> List.keepOks Str.toU64

expect parse exampleInput == [[1000, 2000, 3000], [4000], [5000, 6000], [7000, 8000, 9000], [10000]]
