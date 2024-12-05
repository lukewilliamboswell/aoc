app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.9.0/w8YKp2YAgQt5REYk912HfKAHBjcXsrnvtjI0CBzoAT4.tar.br",
    aoc: "https://github.com/lukewilliamboswell/aoc-template/releases/download/0.2.0/tlS1ZkwSKSB87_3poSOXcwHyySe0WxWOWQbPmp7rxBw.tar.br",
}

import pf.Stdin
import pf.Stdout
import parser.String exposing [parseStr, digits, string, codeunit]
import parser.Parser exposing [Parser, sepBy]
import pf.Utc
import aoc.AoC {
    stdin: Stdin.readToEnd,
    stdout: Stdout.write,
    time: \{} -> Utc.now {} |> Task.map Utc.toMillisSinceEpoch,
}

main = AoC.solve { year: 2024, day: 5, title: "Print Queue", part1, part2 }

part1 : Str -> Result Str [ParsingFailure Str, ParsingIncomplete Str]
part1 = \input ->

    { rules, updates } = try parse (Str.trim input)

    filtered : List (List U64)
    filtered = List.keepIf updates \update -> List.all rules \rule -> checkRule update rule

    middles : List U64
    middles = List.map filtered getMiddle

    middles |> List.sum |> Num.toStr |> Ok

expect part1 exampleInput == Ok "143"

part2 : Str -> Result Str _
part2 = \input ->
    { rules, updates } = try parse (Str.trim input)

    filtered : List (List U64)
    filtered = List.keepIf updates \update -> List.any rules \rule -> !(checkRule update rule)

    reordered = List.map filtered \update -> applyRulesRecursive update rules

    middles : List U64
    middles = List.map reordered getMiddle

    middles |> List.sum |> Num.toStr |> Ok

expect part2 exampleInput == Ok "123"

checkRule : List U64, { before : U64, after : U64 } -> Bool
checkRule = \update, { before, after } ->
    when List.keepIf update \n -> n == before || n == after is
        [a, b] -> a == before && b == after
        _ -> Bool.true

expect checkRule [75, 47, 61, 53, 29] { before: 47, after: 53 }

reorderRule : List U64, { before : U64, after : U64 } -> [Swapped (List U64), NoChange]
reorderRule = \update, { before, after } ->

    init : { before : [Some U64, None], after : [Some U64, None] }
    init = { before: None, after: None }

    idxs =
        List.walkWithIndex update init \state, n, i ->
            if n == before then
                { state & before: Some i }
            else if n == after then
                { state & after: Some i }
            else
                state

    when (idxs.before, idxs.after) is
        (Some b, Some a) if b > a -> List.swap update b a |> Swapped
        _ -> NoChange

expect reorderRule [75, 47, 61, 53, 29] { before: 47, after: 53 } == NoChange
expect reorderRule [75, 53, 61, 47, 29] { before: 47, after: 53 } == Swapped [75, 47, 61, 53, 29]

applyRulesRecursive : List U64, List { before : U64, after : U64 } -> List U64
applyRulesRecursive = \update, rules ->
    List.walkUntil rules NoChange \_, rule ->
        when reorderRule update rule is
            Swapped new -> Break (Swapped new)
            NoChange -> Continue NoChange
    |> \outcome ->
        when outcome is
            Swapped new -> applyRulesRecursive new rules
            NoChange -> update

expect
    update = [75, 47, 61, 53, 29]
    rules = [
        { before: 61, after: 47 },
        { before: 99, after: 47 },
        { before: 47, after: 75 },
    ]
    actual = applyRulesRecursive update rules
    actual == [61, 47, 75, 53, 29]

getMiddle : List U64 -> U64
getMiddle = \numbers ->
    when numbers is
        [middle] -> middle
        _ -> getMiddle (numbers |> List.dropFirst 1 |> List.dropLast 1)

expect getMiddle [1, 2, 3, 4, 5] == 3

parse : Str -> Result { rules : List { before : U64, after : U64 }, updates : List (List U64) } _
parse = \input ->
    parser = { Parser.map2 <-
        rules: sepBy parseRule (codeunit '\n'),
        _: string "\n\n",
        updates: sepBy parseUpdate (codeunit '\n'),
    }

    parseStr parser input

parseRule : Parser _ { before : U64, after : U64 }
parseRule =
    { Parser.map2 <-
        before: digits,
        _: codeunit '|',
        after: digits,
    }

expect parseStr parseRule "47|53" == Ok { before: 47, after: 53 }

parseUpdate : Parser _ (List U64)
parseUpdate = sepBy digits (codeunit ',')

expect parseStr parseUpdate "75,47,61,53,29" == Ok [75, 47, 61, 53, 29]

expect
    actual = parse exampleInput
    actual
    == Ok {
        rules: [
            { before: 47, after: 53 },
            { before: 97, after: 13 },
            { before: 97, after: 61 },
            { before: 97, after: 47 },
            { before: 75, after: 29 },
            { before: 61, after: 13 },
            { before: 75, after: 53 },
            { before: 29, after: 13 },
            { before: 97, after: 29 },
            { before: 53, after: 29 },
            { before: 61, after: 53 },
            { before: 97, after: 53 },
            { before: 61, after: 29 },
            { before: 47, after: 13 },
            { before: 75, after: 47 },
            { before: 97, after: 75 },
            { before: 47, after: 61 },
            { before: 75, after: 61 },
            { before: 47, after: 29 },
            { before: 75, after: 13 },
            { before: 53, after: 13 },
        ],
        updates: [
            [75, 47, 61, 53, 29],
            [97, 61, 53, 29, 13],
            [75, 29, 13],
            [75, 97, 47, 61, 53],
            [61, 13, 29],
            [97, 13, 75, 29, 47],
        ],
    }

exampleInput =
    """
    47|53
    97|13
    97|61
    97|47
    75|29
    61|13
    75|53
    29|13
    97|29
    53|29
    61|53
    97|53
    61|29
    47|13
    75|47
    97|75
    47|61
    75|61
    47|29
    75|13
    53|13

    75,47,61,53,29
    97,61,53,29,13
    75,29,13
    75,97,47,61,53
    61,13,29
    97,13,75,29,47
    """
