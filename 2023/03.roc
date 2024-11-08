app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.16.0/O00IPk-Krg_diNS2dVWlI0ZQP794Vctxzv0ha96mK0E.tar.br",
    aoc: "https://github.com/lukewilliamboswell/aoc-template/releases/download/0.1.0/DcTQw_U67F22cX7pgx93AcHz_ShvHRaFIFjcijF3nz0.tar.br",
}

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
        year: 2023,
        day: 3,
        title: "Gear Ratios",
        part1,
        part2,
    }

exampleInput =
    """
    467..114..
    ...*......
    ..35..633.
    ......#...
    617*......
    .....+.58.
    ..592.....
    ......755.
    ...$.*....
    .664.598..
    """

Location : { row : U64, col : U64 }
Token : [Digit U8, Dot,  Symbol [Asterisk, Slash, Equal, Ampersand, At, Plus, Hash, Minus, Dollar, Percent], Number U64]
LocationToken : { loc : Location, token : Token}
Schematic : Dict Location Token

part1 : Str -> Result Str [NotImplemented, Error Str]
part1 = \input ->

    inputSchematic : Schematic
    inputSchematic =
        input
        |> parseLocationTokens
        |> filterDots
        |> toSchematic
        |> replaceDigitsWithNumbers

    sum =
        inputSchematic
        |> symbolLocations
        |> List.map (adjacentLocations inputSchematic)
        |> List.map filterUniqueNumbers
        |> List.map sumPartNumbers # assume each number can only see 1 symbol, otherwise we will double count
        |> List.sum

    Ok "The sum of all of the part numbers in the engine schematic is $(Num.toStr sum)"

expect part1 exampleInput == Ok "The sum of all of the part numbers in the engine schematic is 4361"

part2 : Str -> Result Str [NotImplemented, Error Str]
part2 = \input ->

    inputSchematic : Schematic
    inputSchematic =
        input
        |> parseLocationTokens
        |> filterDots
        |> toSchematic
        |> replaceDigitsWithNumbers

    sum =
        inputSchematic
        |> gearLocations
        |> List.map (adjacentLocations inputSchematic)
        |> List.map filterUniqueNumbers
        |> List.keepIf \lts -> hasExactlyTwoParts lts []
        |> List.map calculateGearRatio
        |> List.sum

    Ok "The the sum of all of the gear ratios in the engine schematic is $(Num.toStr sum)"

expect part2 exampleInput == Ok "The the sum of all of the gear ratios in the engine schematic is 467835"

parseLocationTokens : Str -> List (List LocationToken)
parseLocationTokens = \input ->
    input |> Str.split "\n" |> List.mapWithIndex \rowStr, row ->
        rowStr |> Str.toUtf8 |> List.mapWithIndex \byte, col ->
            {loc : {row, col}, token: tokenFromByte byte }

tokenFromByte : U8 -> Token
tokenFromByte = \b ->
    when b is
        a if a >= '0' && a <= '9' -> Digit (a - '0')
        '*' -> Symbol Asterisk
        '/' -> Symbol Slash
        '=' -> Symbol Equal
        '&' -> Symbol Ampersand
        '@' -> Symbol At
        '#' -> Symbol Hash
        '+' -> Symbol Plus
        '-' -> Symbol Minus
        '$' -> Symbol Dollar
        '%' -> Symbol Percent
        '.' -> Dot
        _ ->
            str = [b] |> Str.fromUtf8 |> Result.withDefault ""
            crash "token '$(str)' not recognised "

expect tokenFromByte '.' == Dot
expect tokenFromByte '2' == Digit 2u8
expect tokenFromByte '$' == Symbol Dollar

isDot : {token : Token}a -> Bool
isDot = \{token} -> token != Dot

expect !(isDot {loc: {row: 0, col: 0}, token: Dot})
expect isDot {loc: {row: 0, col: 0}, token: Digit 0}

filterDots : List (List LocationToken) -> List LocationToken
filterDots = \lts ->
    lts |> List.join |> List.keepIf isDot

toSchematic : List LocationToken -> Schematic
toSchematic = \lts ->
    List.walk lts (Dict.empty {}) \dict, lt ->
        Dict.insert dict lt.loc lt.token

symbolLocations : Schematic -> List Location
symbolLocations = \schematic ->
    Dict.walk schematic [] \acc, loc, token ->
        when token is
            Symbol _ -> List.append acc loc
            _ -> acc

# increasing row is down
# increasing col is right
move : Location -> ([UpLeft, UpRight, DownLeft, DownRight, Left, Up, Right, Down] -> Result Location [Invalid])
move = \{row, col} ->
    \direction ->
        maybeRow =
            when direction is
                UpLeft -> Num.subChecked row 1
                UpRight -> Num.subChecked row 1
                DownLeft -> Num.addChecked row 1
                DownRight -> Num.addChecked row 1
                Left -> Ok row
                Up -> Num.subChecked row 1
                Right -> Ok row
                Down -> Num.addChecked row 1

        maybeCol =
            when direction is
                UpLeft -> Num.subChecked col 1
                UpRight -> Num.addChecked col 1
                DownLeft -> Num.subChecked col 1
                DownRight -> Num.addChecked col 1
                Left -> Num.subChecked col 1
                Up -> Ok col
                Right -> Num.addChecked col 1
                Down -> Ok col

        when (maybeRow, maybeCol) is
            (Ok r, Ok c) -> Ok {row: r, col: c}
            _ -> Err Invalid

getToken : Schematic -> (Location -> Result LocationToken [NothingAtLocation])
getToken = \schematic -> \loc ->
    when Dict.get schematic loc is
        Ok token -> Ok {loc, token}
        Err KeyNotFound -> Err NothingAtLocation

adjacentLocations : Schematic -> (Location -> List LocationToken)
adjacentLocations = \schematic -> \curr ->
    [UpLeft,UpRight,DownLeft,DownRight,Left,Up,Right,Down]
    |> List.keepOks (move curr)
    |> List.keepOks (getToken schematic)

# take a schematic and convert any digits into the number at that location
replaceDigitsWithNumbers : Schematic -> Schematic
replaceDigitsWithNumbers = \schematic ->
    Dict.map schematic \loc, token ->
        when token is
            Digit _ -> Number (getNumberAtLocation schematic loc WalkingLeft)
            _ -> token

# start with a digit, walk left to first digit, then walk right building up the number
getNumberAtLocation : Schematic, Location, [WalkingLeft, BuildingNumber U64] -> U64
getNumberAtLocation = \schematic, loc, state ->

    maybeRight =
        (move loc) Right
        |> Result.try \right ->
            when Dict.get schematic right is
                Ok (Digit u8) -> Ok (right, u8)
                _ -> Err Invalid

    maybeLeft =
        (move loc) Left
        |> Result.try \left ->
            when Dict.get schematic left is
                Ok (Digit u8) -> Ok (left, u8)
                _ -> Err Invalid

    maybeCurrent =
        when Dict.get schematic loc is
            Ok (Digit u8) -> Ok (loc, u8)
            _ -> Err Invalid

    when (state,             maybeLeft,    maybeRight,     maybeCurrent ) is
        (WalkingLeft,        Ok (left, _), _,              Ok _         ) -> getNumberAtLocation schematic left WalkingLeft
        (WalkingLeft,        _,            _,              Err _        ) -> crash "starting location isn't a digit or isn't in schematic"
        (WalkingLeft,        _,            Ok (right, _),  Ok (_, u8)   ) -> getNumberAtLocation schematic right (BuildingNumber (Num.toU64 u8))
        (WalkingLeft,        _,            _,              Ok (_, u8)   ) -> Num.toU64 u8 # single number
        (WalkingLeft,        _,            _,              _            ) -> crash "unable to move right"
        (BuildingNumber u64, _,            Ok (right, _),  Ok (_, u8)   ) -> getNumberAtLocation schematic right (BuildingNumber ((u64 * 10) + (Num.toU64 u8)))
        (BuildingNumber u64, _,            _,              Ok (_, u8)   ) -> (u64 * 10) + (Num.toU64 u8) # base case
        (BuildingNumber _,   _,            _,              _            ) -> crash "moved to a location that isnt a number"

# take a list of location/tolen pairs and keep only unique numbers
filterUniqueNumbers : List LocationToken -> List LocationToken
filterUniqueNumbers = \lts ->
    filterUniqueNumbersHelp lts [] []

filterUniqueNumbersHelp : List LocationToken, List U64, List LocationToken -> List LocationToken
filterUniqueNumbersHelp = \lts, seen, keep ->
    next = List.dropFirst lts 1
    when lts is
        [] -> keep # base case, keep these locations
        [first, ..] ->
            when first.token is
                Number u64 if !(List.contains seen u64) -> # we haven't seen this number before
                    filterUniqueNumbersHelp
                        next
                        (List.append seen u64)
                        (List.append keep first)
                _ -> filterUniqueNumbersHelp next seen keep # ignore anything that is not a number

sumPartNumbers : List LocationToken -> U64
sumPartNumbers = \lts ->
    next = List.dropFirst lts 1
    when lts is
        [] -> 0 # base case
        [first, ..] ->
            when first.token is
                Number u64 -> u64 + (sumPartNumbers next) # next
                _ -> crash "expected only numbers"

gearLocations : Schematic -> List Location
gearLocations = \schematic ->
    Dict.walk schematic [] \acc, loc, token  ->
        when token is
            Symbol Asterisk -> List.append acc loc
            _ -> acc

hasExactlyTwoParts : List LocationToken, List U64 -> Bool
hasExactlyTwoParts = \lts, seen ->
    next = List.dropFirst lts 1
    when lts is
        [] -> List.len seen == 2
        [first, .. ] ->
            when first.token is
                Number u64 -> hasExactlyTwoParts next (List.append seen u64)
                _ -> crash "expect only numbers"

calculateGearRatio : List LocationToken -> U64
calculateGearRatio = \lts ->

    expect List.len lts <= 2

    next = List.dropFirst lts 1
    when lts is
        [] -> 1 # base case
        [first, ..] ->
            when first.token is
                Number u64 -> u64 * (calculateGearRatio next) # next
                _ -> crash "expected only numbers"
