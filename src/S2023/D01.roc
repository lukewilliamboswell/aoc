interface S2023.D01
    exposes [solution]
    imports [
        AoC,
        "2023-01.txt" as input : Str,
    ]

solution : AoC.Solution
solution = { year: 2023, day: 1, title: "Trebuchet?!", part1, part2 }

isDigit : U8 -> Bool
isDigit = \b -> 
    b == '0' || 
    b == '1' || 
    b == '2' || 
    b == '3' || 
    b == '4' || 
    b == '5' || 
    b == '6' || 
    b == '7' || 
    b == '8' || 
    b == '9'

parsePart1 : Str -> List (List U8)
parsePart1 = \str ->
    str 
    |> Str.split "\n" 
    |> List.map Str.toUtf8 
    |> List.map \bytes -> bytes |> List.keepIf isDigit

toCalibration : List U8 -> U32        
toCalibration = \digits ->
    when (List.first digits, List.last digits) is 
        (Ok first, Ok last) -> 
            when [first, last] |> Str.fromUtf8 |> Result.try Str.toU32 is 
                Ok n -> n   
                Err _ -> crash "couldnt parse number"
        _ -> crash "expected at least one number"
    
expect 
    example 
    |> parsePart1 
    |> List.map toCalibration 
    |> List.sum
    |> Bool.isEq 142


part1 : {} -> Result Str [NotImplemented, Error Str]
part1 = \_ -> 

    vals = 
        input 
        |> parsePart1
        |> List.map toCalibration 

    Ok "sum of all of the calibration values \(vals |> List.sum |> Num.toStr)"
    
example =
    """
    1abc2
    pqr3stu8vwx
    a1b2c3d4e5f
    treb7uchet
    """

expect 
    [
        ['1','2'],
        ['3','8'],
        ['1','2', '3', '4', '5'],
        ['7'],
    ]
    |> Bool.isEq (parsePart1 example)

takeDigits : {digits: List U8, rest : List U8} -> {digits: List U8, rest : List U8}
takeDigits = \{digits, rest} -> 
    when rest is 
        [] -> {digits, rest}
        [a, ..] if isDigit a -> takeDigits { digits : List.append digits a, rest : List.dropFirst rest 1}
        ['o','n','e',..] -> takeDigits { digits : List.append digits '1', rest : List.dropFirst rest 1}
        ['t','w','o',..] -> takeDigits { digits : List.append digits '2', rest : List.dropFirst rest 1}
        ['t','h','r','e','e',..] -> takeDigits { digits : List.append digits '3', rest : List.dropFirst rest 1}
        ['f','o','u','r',..] -> takeDigits { digits : List.append digits '4', rest : List.dropFirst rest 1}
        ['f','i','v','e',..] -> takeDigits { digits : List.append digits '5', rest : List.dropFirst rest 1}
        ['s','i','x',..] -> takeDigits { digits : List.append digits '6', rest : List.dropFirst rest 1}
        ['s','e','v','e','n',..] -> takeDigits { digits : List.append digits '7', rest : List.dropFirst rest 1}
        ['e','i','g','h','t',..] -> takeDigits { digits : List.append digits '8', rest : List.dropFirst rest 1}
        ['n','i','n','e',..] -> takeDigits { digits : List.append digits '9', rest : List.dropFirst rest 1}
        _ -> takeDigits { digits, rest : List.dropFirst rest 1}

part2Example =
    """
    two1nine
    eightwothree
    abcone2threexyz
    xtwone3four
    4nineeightseven2
    zoneight234
    7pqrstsixteen
    """

expect
    part2Example 
    |> parsePart2 
    |> List.map toCalibration 
    |> List.sum
    |> Bool.isEq 281

expect  
    [
        ['2','1','9'], # 29
        ['8','2','3'], # 83
        ['1','2','3'], # 13
        ['2','1','3','4'], # 24
        ['4','9','8','7','2'], # 42
        ['1','8','2','3','4'], # 14
        ['7','6'], # 76
    ]
    |> Bool.isEq (parsePart2 part2Example)

parsePart2 : Str -> List (List U8)
parsePart2 = \str -> 
    str 
    |> Str.split "\n" 
    |> List.map Str.toUtf8 
    |> List.map \bytes -> takeDigits { digits : [], rest : bytes}
    |> List.map .digits

part2 : {} -> Result Str [NotImplemented, Error Str]
part2 = \_ -> 

    vals = 
        input 
        |> parsePart2 
        |> List.map toCalibration 
    
    Ok "sum of all of the calibration values \(vals |> List.sum |> Num.toStr)"
