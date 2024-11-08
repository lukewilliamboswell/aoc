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
import parser.String exposing [string, digits, parseStr, codeunit]
import parser.Parser exposing [Parser, map, sepBy, const, keep, skip, oneOf]

main =
    AoC.solve {
        year: 2023,
        day: 2,
        title: "Cube Conundrum",
        part1,
        part2,
    }

part1 : Str -> Result Str _
part1 = \input ->

    games = parseStr? (sepBy parseGame (codeunit '\n')) input

    ids =
        List.walkWithIndex games [] \validGames, game, idx ->
            if isValidGame game then
                List.append validGames (Num.toU32 idx + 1)
            else
                validGames

    Ok "The sum of the IDs is $(ids |> List.sum |> Num.toStr)"

part2 : Str -> Result Str _
part2 = \input ->

    games = parseStr? (sepBy parseGame (codeunit '\n')) input

    totalPower =
        List.walkWithIndex games 0 \sum, game, _ ->

            { red, green, blue } = calcGameMinCubeSet game

            power = red * green * blue

            sum + power

    Ok "The sum of the power is $(Num.toStr totalPower)"

parseNumberColor : Parser (List U8) [Red U32, Green U32, Blue U32]
parseNumberColor =
    const
        (\number -> \color ->
                when color is
                    Red -> Red number
                    Green -> Green number
                    Blue -> Blue number
        )
    |> skip (codeunit ' ')
    |> keep (digits |> map Num.toU32)
    |> skip (codeunit ' ')
    |> keep
        (
            oneOf [
                string "red" |> map \_ -> Red,
                string "green" |> map \_ -> Green,
                string "blue" |> map \_ -> Blue,
            ]
        )

expect parseStr parseNumberColor " 64 green" == Ok (Green 64u32)
expect parseStr parseNumberColor " 12 red" == Ok (Red 12u32)
expect parseStr parseNumberColor " 546 blue" == Ok (Blue 546u32)

parseGame : Parser (List U8) (List (List [Red U32, Green U32, Blue U32]))
parseGame =
    const (\i -> i)
    |> skip (string "Game ")
    |> skip (digits)
    |> skip (codeunit ':')
    |> keep
        (
            parseNumberColor
            |> sepBy (codeunit ',')
            |> sepBy (codeunit ';')
        )

expect
    parseStr parseGame "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
    == Ok [
        [Blue 3u32, Red 4u32],
        [Red 1u32, Green 2u32, Blue 6u32],
        [Green 2u32],
    ]

isValidGame : List (List [Red U32, Green U32, Blue U32]) -> Bool
isValidGame = \subsets ->
    when subsets is
        [] -> Bool.true
        [subset, ..] ->
            if isValidSubset subset then
                isValidGame (List.dropFirst subsets 1)
            else
                Bool.false

expect isValidGame [[Blue 3u32, Red 4u32], [Red 1u32, Green 2u32, Blue 6u32], [Green 2u32]]
expect !(isValidGame [[Blue 15u32]])

isValidSubset : List [Red U32, Green U32, Blue U32] -> Bool
isValidSubset = \subsubset ->
    next = List.dropFirst subsubset 1
    when subsubset is
        [] -> Bool.true
        [Red count, ..] if count > 12 -> Bool.false
        [Green count, ..] if count > 13 -> Bool.false
        [Blue count, ..] if count > 14 -> Bool.false
        _ -> isValidSubset next

expect isValidSubset [Red 1u32, Green 2u32, Blue 6u32]
expect !(isValidSubset [Red 100u32, Green 2u32, Blue 6u32])

calcGameMinCubeSet : List (List [Red U32, Green U32, Blue U32]) -> { red : U32, green : U32, blue : U32 }
calcGameMinCubeSet = \subsets ->
    List.walk subsets { red: 0, green: 0, blue: 0 } calcMinCubeSetHelp

calcMinCubeSetHelp : { red : U32, green : U32, blue : U32 }, List [Red U32, Green U32, Blue U32] -> { red : U32, green : U32, blue : U32 }
calcMinCubeSetHelp = \current, subsubsets ->
    next = List.dropFirst subsubsets 1
    when subsubsets is
        [] -> current
        [Red count, ..] if count > current.red -> calcMinCubeSetHelp { current & red: count } next
        [Green count, ..] if count > current.green -> calcMinCubeSetHelp { current & green: count } next
        [Blue count, ..] if count > current.blue -> calcMinCubeSetHelp { current & blue: count } next
        _ -> calcMinCubeSetHelp current next

expect
    game = parseStr parseGame "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green" |> Result.withDefault []
    minCubes = calcGameMinCubeSet game
    minCubes == { red: 4, green: 2, blue: 6 }
