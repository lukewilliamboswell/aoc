interface S2023.D02
    exposes [solution]
    imports [
        AoC,
        Parser.Core.{ Parser, const, map, keep, skip, oneOf, sepBy },
        Parser.String.{ parseStr, string, digits, codeunit },
        "2023-02.txt" as input : Str,
    ]

solution : AoC.Solution
solution = { year: 2023, day: 2, title: "Cube Conundrum", part1, part2 }

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

part1 : {} -> Result Str [NotImplemented, Error Str]
part1 = \_ -> 

    games <- 
        parseStr (sepBy parseGame (codeunit '\n')) input
        |> Result.mapErr \_ -> Error "unable to parse input"
        |> Result.try 

    ids = 
        List.walkWithIndex games [] \validGames, game, idx ->
            if isValidGame game then
                List.append validGames (Num.toU32 idx + 1)
            else
                validGames
    
    Ok "sum of the IDs is \(ids |> List.sum |> Num.toStr)"

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

part2 : {} -> Result Str [NotImplemented, Error Str]
part2 = \_ -> 
    games <- 
        parseStr (sepBy parseGame (codeunit '\n')) input
        |> Result.mapErr \_ -> Error "unable to parse input"
        |> Result.try 

    totalPower = 
        sum, game, _ <- games |> List.walkWithIndex 0

        { red, green, blue } = calcGameMinCubeSet game

        power = red * green * blue

        sum + power

    Ok "The sum of the power is \(Num.toStr totalPower)"
    