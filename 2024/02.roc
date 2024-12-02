app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.9.0/w8YKp2YAgQt5REYk912HfKAHBjcXsrnvtjI0CBzoAT4.tar.br",
    aoc: "https://github.com/lukewilliamboswell/aoc-template/releases/download/0.2.0/tlS1ZkwSKSB87_3poSOXcwHyySe0WxWOWQbPmp7rxBw.tar.br",
}

import pf.Stdin
import pf.Stdout
import pf.Utc
import parser.String exposing [digits, parseStr, codeunit]
import parser.Parser exposing [Parser, sepBy]
import aoc.AoC {
    stdin: Stdin.readToEnd,
    stdout: Stdout.write,
    time: \{} -> Utc.now {} |> Task.map Utc.toMillisSinceEpoch,
}

main = AoC.solve { year: 2024, day: 2, title: "Historian Hysteria", part1, part2 }

part1 = \input ->

    reports = try parseStr (sepBy parseReport (codeunit '\n')) (Str.trim input)

    count = reports |> List.countIf isSafe |> Num.toStr

    Ok "$(count) many reports are safe!"

part2 = \input ->

    reports = try parseStr (sepBy parseReport (codeunit '\n')) (Str.trim input)

    count = reports |> List.countIf isSafeTolerant |> Num.toStr

    Ok "$(count) many reports are safe!"

expect
    a = part2 exampleInput
    a == Ok "4 many reports are safe!"

exampleInput =
    """
    7 6 4 2 1
    1 2 7 8 9
    9 7 6 2 1
    1 3 2 4 5
    8 6 4 4 1
    1 3 6 7 9
    """

parseReport : Parser _ (List U64)
parseReport = sepBy digits (codeunit ' ')

expect parseStr parseReport "7 6 4 2 1" == Ok [7, 6, 4, 2, 1]

isIncreasingSafely : List U64 -> Bool
isIncreasingSafely = \levels ->
    when levels is
        [first, .. as rest] ->
            List.walkUntil rest (Ok first) \maybePrev, curr ->

                prev = unwrap maybePrev

                if curr > prev && safeDifference curr prev then
                    Continue (Ok curr)
                else
                    Break (Err {})
            |> Result.isOk

        _ -> crash "expected at least one level"

expect isIncreasingSafely [1, 2, 3, 4, 5]

isDecreasingSafely : List U64 -> Bool
isDecreasingSafely = \levels ->
    when levels is
        [first, .. as rest] ->
            List.walkUntil rest (Ok first) \maybePrev, curr ->

                prev = unwrap maybePrev

                if curr < prev && safeDifference curr prev then
                    Continue (Ok curr)
                else
                    Break (Err {})
            |> Result.isOk

        _ -> crash "expected at least one level"

expect isIncreasingSafely [1, 2, 3, 4, 5]

safeDifference = \curr, prev ->
    diff = Num.absDiff curr prev
    diff >= 1 && diff <= 3

expect safeDifference 1 2
expect safeDifference 1 4
expect !(safeDifference 1 1)
expect !(safeDifference 1 5)

isSafe = \levels -> isIncreasingSafely levels || isDecreasingSafely levels

expect isSafe [7, 6, 4, 2, 1]
expect !(isSafe [1, 2, 7, 8, 9])
expect !(isSafe [9, 7, 6, 2, 1])
expect !(isSafe [1, 3, 2, 4, 5])
expect !(isSafe [8, 6, 4, 4, 1])
expect isSafe [1, 3, 6, 7, 9]

isIncreasingSafelyTolerant : List U64 -> Bool
isIncreasingSafelyTolerant = \levels ->
    when levels is
        [first, .. as rest] ->
            List.walkUntil rest (Ok (NilFail first)) \maybePrev, curr ->
                when unwrap maybePrev is
                    NilFail prev ->
                        if curr > prev && safeDifference curr prev then
                            Continue (Ok (NilFail curr))
                        else
                            Continue (Ok (OneFail prev))

                    OneFail prev ->
                        if curr > prev && safeDifference curr prev then
                            Continue (Ok (OneFail curr))
                        else
                            Break (Err {})
            |> Result.isOk

        _ -> crash "expected at least one level"

isDecreasingSafelyTolerant : List U64 -> Bool
isDecreasingSafelyTolerant = \levels ->
    when levels is
        [first, .. as rest] ->
            List.walkUntil rest (Ok (NilFail first)) \maybePrev, curr ->
                when unwrap maybePrev is
                    NilFail prev ->
                        if curr < prev && safeDifference curr prev then
                            Continue (Ok (NilFail curr))
                        else
                            Continue (Ok (OneFail prev))

                    OneFail prev ->
                        if curr < prev && safeDifference curr prev then
                            Continue (Ok (OneFail curr))
                        else
                            Break (Err {})
            |> Result.isOk

        _ -> crash "expected at least one level"

isSafeTolerant = \levels -> isIncreasingSafelyTolerant levels || isDecreasingSafelyTolerant levels

expect isSafeTolerant [7, 6, 4, 2, 1]
expect !(isSafeTolerant [1, 2, 7, 8, 9])
expect !(isSafeTolerant [9, 7, 6, 2, 1])
expect isSafeTolerant [1, 3, 2, 4, 5]
expect isSafeTolerant [8, 6, 4, 4, 1]
expect isSafeTolerant [1, 3, 6, 7, 9]

expect isSafeTolerant [1, 3, 3, 6, 7, 9]

unwrap = \result ->
    when result is
        Ok value -> value
        Err _ -> crash "expected Ok"
