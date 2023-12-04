interface S2023.D01
    exposes [solution]
    imports [
        AoC,
        "2023-01.txt" as puzzleInput : Str,
    ]

solution : AoC.Solution
solution = { year: 2023, day: 1, title: "Trebuchet?!", part1, part2, puzzleInput }

exampleInputPart1 =
    """
    1abc2
    pqr3stu8vwx
    a1b2c3d4e5f
    treb7uchet
    """

exampleInputPart2 =
    """
    two1nine
    eightwothree
    abcone2threexyz
    xtwone3four
    4nineeightseven2
    zoneight234
    7pqrstsixteen
    """

part1 : Str -> Result Str [NotImplemented, Error Str]
part1 = \input ->

    vals =
        input
        |> Str.split "\n"
        |> List.map Str.toUtf8
        |> List.map \bytes -> bytes |> List.keepIf isDigit
        |> List.map toCalibration

    Ok "The sum of all of the calibration values \(vals |> List.sum |> Num.toStr)"

expect part1 exampleInputPart1 == Ok "The sum of all of the calibration values 142"

part2 : Str -> Result Str [NotImplemented, Error Str]
part2 = \input ->

    sum =
        input
        |> Str.split "\n"
        |> List.map Str.toUtf8
        |> List.map \bytes -> takeDigits { digits: [], rest: bytes }
        |> List.map toCalibration
        |> List.sum

    Ok "The sum of all of the calibration values \(Num.toStr sum)"

expect part2 exampleInputPart2 == Ok "The sum of all of the calibration values 281"

isDigit : U8 -> Bool
isDigit = \b -> b >= '0' && b <= '9'

toCalibration : List U8 -> U32
toCalibration = \digits ->
    when (List.first digits, List.last digits) is
        (Ok first, Ok last) ->
            when [first, last] |> Str.fromUtf8 |> Result.try Str.toU32 is
                Ok n -> n
                Err _ -> crash "couldnt parse number"

        _ -> crash "expected at least one number"

takeDigits : { digits : List U8, rest : List U8 } -> List U8
takeDigits = \{ digits, rest } ->
    when rest is
        [] -> digits
        [a, ..] if isDigit a -> takeDigits { digits: List.append digits a, rest: List.dropFirst rest 1 }
        ['o', 'n', 'e', ..] -> takeDigits { digits: List.append digits '1', rest: List.dropFirst rest 1 }
        ['t', 'w', 'o', ..] -> takeDigits { digits: List.append digits '2', rest: List.dropFirst rest 1 }
        ['t', 'h', 'r', 'e', 'e', ..] -> takeDigits { digits: List.append digits '3', rest: List.dropFirst rest 1 }
        ['f', 'o', 'u', 'r', ..] -> takeDigits { digits: List.append digits '4', rest: List.dropFirst rest 1 }
        ['f', 'i', 'v', 'e', ..] -> takeDigits { digits: List.append digits '5', rest: List.dropFirst rest 1 }
        ['s', 'i', 'x', ..] -> takeDigits { digits: List.append digits '6', rest: List.dropFirst rest 1 }
        ['s', 'e', 'v', 'e', 'n', ..] -> takeDigits { digits: List.append digits '7', rest: List.dropFirst rest 1 }
        ['e', 'i', 'g', 'h', 't', ..] -> takeDigits { digits: List.append digits '8', rest: List.dropFirst rest 1 }
        ['n', 'i', 'n', 'e', ..] -> takeDigits { digits: List.append digits '9', rest: List.dropFirst rest 1 }
        _ -> takeDigits { digits, rest: List.dropFirst rest 1 }

