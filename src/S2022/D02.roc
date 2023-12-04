interface S2022.D02
    exposes [solution]
    imports [
        AoC,
        Parser.Core.{ Parser, const, keep, skip, oneOf, sepBy },
        Parser.String.{ parseStr, codeunit },
        "2022-02.txt" as puzzleInput : Str,
    ]

solution : AoC.Solution
solution = { year: 2022, day: 2, title: "Rock Paper Scissors", part1, part2, puzzleInput }

exampleInput =
    """
    A Y
    B X
    C Z
    """

part1 : Str -> Result Str [NotImplemented, Error Str]
part1 = \input ->

    rounds <-
        parseStr (sepBy rockPaperScissorParser (codeunit '\n')) input
        |> Result.mapErr \_ -> Error "failed to parse input"
        |> Result.map

    total =
        rounds
        |> List.map \round ->
            round
            |> determineChoice
            |> calculateScore
        |> List.sum

    "The total score following guide \(Num.toStr total)"

expect part1 exampleInput == Ok "The total score following guide 15"

part2 : Str -> Result Str [NotImplemented, Error Str]
part2 = \input ->

    rounds <-
        parseStr (sepBy rockPaperScissorParser (codeunit '\n')) input
        |> Result.mapErr \_ -> Error "failed to parse input"
        |> Result.map

    total =
        rounds
        |> List.map \round ->
            round
            |> determineChoice
            |> calculateScore
        |> List.sum

    "The total score following guide \(Num.toStr total)"

expect part2 exampleInput == Ok "The total score following guide 12"

RockScissorPaper : [Rock, Scissor, Paper]
LossDrawWin : [Loss, Draw, Win]
OpponentGuide : { opponent : RockScissorPaper, guide : LossDrawWin }
OpponentChoice : { opponent : RockScissorPaper, choice : RockScissorPaper }

calculateScore : OpponentChoice -> U64
calculateScore = \oc ->
    baseScore =
        when oc.choice is
            Rock -> 1
            Paper -> 2
            Scissor -> 3

    winLossDrawScore =
        when determineOutcome oc is
            Loss -> 0
            Draw -> 3
            Win -> 6

    baseScore + winLossDrawScore

determineOutcome : OpponentChoice -> LossDrawWin
determineOutcome = \oc ->
    when (oc.opponent, oc.choice) is
        (Rock, Rock) -> Draw
        (Rock, Paper) -> Win
        (Rock, Scissor) -> Loss
        (Paper, Rock) -> Loss
        (Paper, Paper) -> Draw
        (Paper, Scissor) -> Win
        (Scissor, Rock) -> Win
        (Scissor, Paper) -> Loss
        (Scissor, Scissor) -> Draw

determineChoice : OpponentGuide -> OpponentChoice
determineChoice = \{ opponent, guide } ->
    if determineOutcome { opponent, choice: Rock } == guide then
        { opponent, choice: Rock }
    else if determineOutcome { opponent, choice: Paper } == guide then
        { opponent, choice: Paper }
    else
        { opponent, choice: Scissor }

rockPaperScissorParser : Parser (List U8) OpponentGuide
rockPaperScissorParser =
    const (\opponent -> \guide -> { opponent, guide })
    |> keep
        (
            oneOf [
                const Rock |> skip (codeunit 'A'),
                const Paper |> skip (codeunit 'B'),
                const Scissor |> skip (codeunit 'C'),
            ]
        )
    |> skip (codeunit ' ')
    |> keep
        (
            oneOf [
                const Loss |> skip (codeunit 'X'),
                const Draw |> skip (codeunit 'Y'),
                const Win |> skip (codeunit 'Z'),
            ]
        )

