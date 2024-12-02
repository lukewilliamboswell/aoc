app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.9.0/w8YKp2YAgQt5REYk912HfKAHBjcXsrnvtjI0CBzoAT4.tar.br",
    aoc: "https://github.com/lukewilliamboswell/aoc-template/releases/download/0.2.0/tlS1ZkwSKSB87_3poSOXcwHyySe0WxWOWQbPmp7rxBw.tar.br",
}

import pf.Stdin
import pf.Stdout
import pf.Utc
import parser.String exposing [digits, parseStr, string, codeunit]
import parser.Parser exposing [Parser, sepBy]
import aoc.AoC {
    stdin: Stdin.readToEnd,
    stdout: Stdout.write,
    time: \{} -> Utc.now {} |> Task.map Utc.toMillisSinceEpoch,
}

main = AoC.solve { year: 2024, day: 1, title: "Historian Hysteria", part1, part2 }

part1 = \input ->

    numbers = try parseStr (sepBy parseLocationIds (codeunit '\n')) (Str.trim input)

    sortedLists = splitAndSort numbers

    distance = calcDistance sortedLists 0

    Ok "The total distance between the lists is $(Num.toStr distance)."

expect part1 exampleInput == Ok "The total distance between the lists is 11."

part2 = \input ->

    numbers = try parseStr (sepBy parseLocationIds (codeunit '\n')) (Str.trim input)

    sortedLists = splitAndSort numbers

    similarity = calcSimilarity sortedLists 0

    Ok "The similarity score is $(Num.toStr similarity)."

expect part2 exampleInput == Ok "The similarity score is 31."

exampleInput =
    """
    3   4
    4   3
    2   5
    1   3
    3   9
    3   3
    """

parseLocationIds : Parser (List U8) { first : U64, second : U64 }
parseLocationIds =
    { Parser.map2 <-
        first: digits,
        _: string "   ",
        second: digits,
    }

expect parseStr parseLocationIds "3   4" == Ok { first: 3, second: 4 }

splitAndSort : List { first : U64, second : U64 } -> (List U64, List U64)
splitAndSort = \numbers ->
    first = numbers |> List.map .first |> List.sortAsc
    second = numbers |> List.map .second |> List.sortAsc

    (first, second)

calcDistance : (List U64, List U64), U64 -> U64
calcDistance = \(first, second), score ->
    when (first, second) is
        ([], []) -> score
        ([a, .. as restA], [b, .. as restB]) -> calcDistance (restA, restB) (score + (Num.absDiff a b))
        _ -> crash "expected input lists to be the same length"

calcSimilarity : (List U64, List U64), U64 -> U64
calcSimilarity = \(first, second), score ->
    when first is
        [] -> score
        [a, .. as restA] ->
            count = second |> List.countIf \b -> a == b

            increment = a * count

            calcSimilarity (restA, second) (score + increment)
