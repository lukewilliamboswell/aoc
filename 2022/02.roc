app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.16.0/O00IPk-Krg_diNS2dVWlI0ZQP794Vctxzv0ha96mK0E.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.8.0/PCkJq9IGyIpMfwuW-9hjfXd6x-bHb1_OZdacogpBcPM.tar.br",
    aoc: "https://github.com/lukewilliamboswell/aoc-template/releases/download/0.1.0/DcTQw_U67F22cX7pgx93AcHz_ShvHRaFIFjcijF3nz0.tar.br",
}

import parser.String exposing [codeunit]
import parser.Parser exposing [Parser, const, keep, skip, oneOf]
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
        year: 2022,
        day: 2,
        title: "Rock Paper Scissors",
        part1,
        part2,
    }

exampleInput =
    """
    A Y
    B X
    C Z
    """

part1 : Str -> Result Str _
part1 = \input ->
    input
    |> Str.trim
    |> \str -> String.parseStr (Parser.sepBy rockPaperScissorParser (codeunit '\n')) str
    |> Result.mapErr \err -> ParseErr err
    |> Result.map \rounds ->
        total =
            rounds
            |> List.map \round ->
                round
                |> determineChoice
                |> calculateScore
            |> List.sum

        "The total score following guide $(Num.toStr total)"

expect
    result = part1 exampleInput
    result == Ok "The total score following guide 12"

part2 : Str -> Result Str [NotImplemented, Error Str]
part2 = \input ->

    input
    |> Str.trim
    |> \str -> String.parseStr (Parser.sepBy rockPaperScissorParser (codeunit '\n')) str
    |> Result.mapErr \_ -> Error "failed to parse input"
    |> Result.map \rounds  ->

        total =
            rounds
            |> List.map \round ->
                round
                |> determineChoice
                |> calculateScore
            |> List.sum

        "The total score following guide $(Num.toStr total)"

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
