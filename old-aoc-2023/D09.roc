interface S2023.D09
    exposes [solution]
    imports [
        AoC,
        Parser.Core.{ Parser, const, oneOf, keep, skip, sepBy },
        Parser.String.{ parseStr, digits, codeunit },
        "2023-09.txt" as puzzleInput : Str,
    ]

solution : AoC.Solution
solution = { year: 2023, day: 9, title: "Mirage Maintenance", part1, part2, puzzleInput }

part1 : Str -> Result Str [NotImplemented, Error Str]
part1 = \input ->

    histories <-
        parseStr (sepBy historyParser (codeunit '\n')) input
        |> Result.mapErr \_ -> Error "unable to parse input"
        |> Result.try

    nextValues = histories |> List.map (predict Last)

    sum = nextValues |> List.sum

    Ok "The the sum of the LAST extrapolated values \(Num.toStr sum)"

expect
    res = part1 exampleInput
    res == Ok "The the sum of the LAST extrapolated values 114"

part2 : Str -> Result Str [NotImplemented, Error Str]
part2 = \input ->

    histories <-
        parseStr (sepBy historyParser (codeunit '\n')) input
        |> Result.mapErr \_ -> Error "unable to parse input"
        |> Result.try

    nextValues = histories |> List.map (predict First)

    sum = nextValues |> List.sum

    Ok "The the sum of the FIRST extrapolated values \(Num.toStr sum)"

expect part2 exampleInput == Ok "The the sum of the FIRST extrapolated values 2"

exampleInput =
    """
    0 3 6 9 12 15
    1 3 6 10 15 21
    10 13 16 21 30 45
    """

History : List I64

numberParser : Parser (List U8) I64
numberParser =
    const
        (\number ->
            when number is
                Positive nat -> Num.toI64 nat
                Negative nat -> -1 * (Num.toI64 nat)
        )
    |> keep
        (
            oneOf [
                const Positive |> keep digits,
                const Negative |> skip (codeunit '-') |> keep digits,
            ]
        )

historyParser : Parser (List U8) History
historyParser = numberParser |> sepBy (codeunit ' ')

expect parseStr historyParser "0 3 6 9 12 -15" == Ok [0, 3, 6, 9, 12, -15]

predict : [First, Last] -> (History -> I64)
predict = \direction -> \currHistory ->
        if List.sum currHistory == 0 then
            0 # base case
        else
            nextHistory = calcNewHistory currHistory Start []

            when direction is
                First ->
                    curr = List.first currHistory |> unwrap "expected a non-empty list"

                    curr - ((predict direction) nextHistory)

                Last ->
                    curr = List.last currHistory |> unwrap "expected a non-empty list"

                    curr + ((predict direction) nextHistory)

expect (predict Last) [0, 3, 6, 9, 12, 15] == 18
expect (predict Last) [1, 3, 6, 10, 15, 21] == 28
expect (predict Last) [10, 13, 16, 21, 30, 45] == 68
expect (predict First) [10, 13, 16, 21, 30, 45] == 5

calcNewHistory : List I64, [Start, Prev I64], List I64 -> List I64
calcNewHistory = \old, maybePrev, new ->
    next = List.dropFirst old 1
    when (old, maybePrev) is
        ([curr, ..], Start) ->
            calcNewHistory
                next
                (Prev curr)
                (List.withCapacity (List.len next))

        ([curr, ..], Prev prev) ->
            calcNewHistory
                next
                (Prev curr)
                (List.append new (curr - prev))

        (_, _) -> new # base case

expect calcNewHistory [0, 3, 6, 9, 12, 15] Start [] == [3, 3, 3, 3, 3]
expect calcNewHistory [3, 3, 3, 3, 3] Start [] == [0, 0, 0, 0]
expect calcNewHistory [1, 3, 6, 10, 15, 21] Start [] == [2, 3, 4, 5, 6]
expect calcNewHistory [2, 3, 4, 5, 6] Start [] == [1, 1, 1, 1]

unwrap = \thing, msg ->
    when thing is
        Ok unwrapped -> unwrapped
        Err _ -> crash msg
