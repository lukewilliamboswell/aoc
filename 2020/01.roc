app [main] {
    pf: platform "../../basic-cli/platform/main.roc",
    aoc: "https://github.com/lukewilliamboswell/aoc-template/releases/download/0.1.0/DcTQw_U67F22cX7pgx93AcHz_ShvHRaFIFjcijF3nz0.tar.br",
}

import pf.Stdin
import pf.Stdout
import pf.Utc
import aoc.AoC {
    stdin: Stdin.readToEnd,
    stdout: Stdout.write,
    time: \{} -> Utc.now {} |> Task.map Utc.toMillisSinceEpoch,
}

main =
    AoC.solve {
        year: 2020,
        day: 1,
        title: "Report Repair",
        part1,
        part2,
    }

part1 : Str -> Result Str Str
part1 = \input ->

    numbers : List U128
    numbers = parse input

    combined : List { x : U128, y : U128, mul : U128 }
    combined =
        List.joinMap numbers \x ->
            List.keepOks numbers \y ->
                if (x + y) == 2020 then
                    Ok { x, y, mul: x * y }
                else
                    Err NotValid

    combined
    |> List.first
    |> Result.map \{ x, y, mul } -> "$(Num.toStr x) * $(Num.toStr y) = $(Num.toStr mul)"
    |> Result.mapErr \_ -> "Expected at least one pair to have sum of 2020"

part2 : Str -> Result Str Str
part2 = \input ->

    numbers : List U128
    numbers = parse input

    combined : List { x : U128, y : U128, z : U128, mul : U128 }
    combined =
        List.joinMap numbers \x ->
            List.joinMap numbers \y ->
                List.keepOks numbers \z ->
                    if (x + y + z) == 2020 then
                        Ok { x, y, z, mul: x * y * z }
                    else
                        Err NotValid

    combined
    |> List.first
    |> Result.map \{ x, y, z, mul } -> "$(Num.toStr x) * $(Num.toStr y) * $(Num.toStr z) = $(Num.toStr mul)"
    |> Result.mapErr \_ -> "Expected at least one triple to have sum of 2020"

parse : Str -> List U128
parse = \input ->
    input
    |> Str.split "\n"
    |> List.keepOks Str.toU128

expect
    result = part1 example
    result == Ok "1721 * 299 = 514579"

expect
    result = part2 example
    result == Ok "979 * 366 * 675 = 241861950"

expect parse example == [1721, 979, 366, 299, 675, 1456]

example =
    """
    1721
    979
    366
    299
    675
    1456
    """
