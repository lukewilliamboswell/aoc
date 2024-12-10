app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    aoc: "https://github.com/lukewilliamboswell/aoc-template/releases/download/0.2.0/tlS1ZkwSKSB87_3poSOXcwHyySe0WxWOWQbPmp7rxBw.tar.br",
}

import pf.Stdin
import pf.Stdout
import pf.Utc
import aoc.AoC {
    stdin: Stdin.readToEnd,
    stdout: Stdout.write,
    time: \{} -> Utc.now {} |> Task.map Utc.toMillisSinceEpoch,
}

main = AoC.solve { year: 2024, day: 10, title: "Hoof It", part1, part2 }

part1 : Str -> Result Str []
part1 = \input ->

    map = parse_map (Str.trim input)

    trail_heads =
        Dict.walk map [] \heads, pos, height ->
            if height == '0' then
                List.append heads pos
            else
                heads

    List.map trail_heads \head -> score_trail head map
    |> List.sum
    |> Num.toStr
    |> Ok

expect part1 example_map_1 == Ok "2"
expect part1 example_map_2 == Ok "4"
expect part1 example_map_3 == Ok "3"
expect part1 example_map_4 == Ok "36"

part2 : Str -> Result Str _
part2 = \_input ->
    Err TODO

score_trail : { r : U8, c : U8 }, Dict { r : U8, c : U8 } U8 -> U64
score_trail = \head_position, map ->

    help : ({ r : U8, c : U8 }, U8), Set { r : U8, c : U8 } -> List { r : U8, c : U8 }
    help = \(position, height), visited ->
        if height == '9' then
            [position]
        else if Set.contains visited position then
            []
        else
            updated_visited = Set.insert visited position

            [Up, Down, Left, Right]
            |> List.keepOks \direction -> step (position, height) direction map
            |> List.map \next -> help next updated_visited
            |> List.join

    help (head_position, '0') (Set.empty {}) |> Set.fromList |> Set.len

step : ({ r : U8, c : U8 }, U8), [Up, Down, Left, Right], Dict { r : U8, c : U8 } U8 -> Result ({ r : U8, c : U8 }, U8) _
step = \(position, height), direction, map ->

    next_position =
        when direction is
            Up -> { r: Num.subWrap position.r 1, c: position.c }
            Down -> { r: Num.addWrap position.r 1, c: position.c }
            Left -> { r: position.r, c: Num.subWrap position.c 1 }
            Right -> { r: position.r, c: Num.addWrap position.c 1 }

    next_height = (Dict.get map next_position)?

    if (Num.subChecked next_height height) == Ok 1 then
        Ok (next_position, next_height)
    else
        Err Invalid

expect
    map = parse_map example_map_1
    a = step ({ c: 3, r: 0 }, '0') Down map
    a == Ok ({ c: 3, r: 1 }, '1')

expect
    map = parse_map example_map_1
    a = step ({ c: 3, r: 1 }, '1') Up map
    a == Err Invalid

expect
    map = parse_map example_map_1
    a = step ({ c: 3, r: 3 }, '3') Left map
    a == Ok ({ c: 2, r: 3 }, '4')

expect
    map = parse_map example_map_1
    a = step ({ c: 3, r: 3 }, '3') Right map
    a == Ok ({ c: 4, r: 3 }, '4')

parse_map : Str -> Dict { r : U8, c : U8 } U8
parse_map = \input ->
    input
    |> Str.splitOn "\n"
    |> List.walkWithIndex (Dict.empty {}) \dict, row, r ->
        row
        |> Str.toUtf8
        |> List.walkWithIndex dict \inner_dict, b, c ->
            if b != '.' then
                Dict.insert inner_dict { r: Num.intCast r, c: Num.intCast c } b
            else
                inner_dict

example_map_1 =
    """
    ...0...
    ...1...
    ...2...
    6543456
    7.....7
    8.....8
    9.....9
    """

expect
    a = parse_map example_map_1
    a
    == Dict.fromList [
        ({ c: 3, r: 0 }, '0'),
        ({ c: 3, r: 1 }, '1'),
        ({ c: 3, r: 2 }, '2'),
        ({ c: 0, r: 3 }, '6'),
        ({ c: 1, r: 3 }, '5'),
        ({ c: 2, r: 3 }, '4'),
        ({ c: 3, r: 3 }, '3'),
        ({ c: 4, r: 3 }, '4'),
        ({ c: 5, r: 3 }, '5'),
        ({ c: 6, r: 3 }, '6'),
        ({ c: 0, r: 4 }, '7'),
        ({ c: 6, r: 4 }, '7'),
        ({ c: 0, r: 5 }, '8'),
        ({ c: 6, r: 5 }, '8'),
        ({ c: 0, r: 6 }, '9'),
        ({ c: 6, r: 6 }, '9'),
    ]

example_map_2 =
    """
    ..90..9
    ...1.98
    ...2..7
    6543456
    765.987
    876....
    987....
    """

example_map_3 =
    """
    10..9..
    2...8..
    3...7..
    4567654
    ...8..3
    ...9..2
    .....01
    """

example_map_4 =
    """
    89010123
    78121874
    87430965
    96549874
    45678903
    32019012
    01329801
    10456732
    """
