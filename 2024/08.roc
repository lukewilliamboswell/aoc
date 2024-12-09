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

main = AoC.solve { year: 2024, day: 8, title: "Resonant Collinearity", part1, part2 }

part1 : Str -> Result Str []
part1 = \input ->

    { maxR, maxC, ax } = parse_antennas (Str.trim input)

    in_map = \{ r, c } -> r >= 0 && r <= maxR && c >= 0 && c <= maxC

    unique_locations =
        Dict.walk ax (Set.empty {}) \unique, _freq, antenna_locations ->
            antenna_locations
            |> to_pairs (Set.empty {})
            |> Set.map anti_nodes
            |> Set.walk unique \set, (a1, a2) ->
                set
                |> Set.insert a1
                |> Set.insert a2

    unique_locations
    |> Set.keepIf in_map
    |> Set.len
    |> Num.toStr
    |> Ok

expect
    actual = part1 example_input
    actual == Ok "14"

part2 : Str -> Result Str _
part2 = \_input ->

    # TODO
    0
    |> Num.toStr
    |> Ok

expect
    actual = part2 example_input
    actual == Ok "34"

anti_nodes : ({ r : U64, c : U64 }, { r : U64, c : U64 }) -> ({ r : U64, c : U64 }, { r : U64, c : U64 })
anti_nodes = \(a, b) ->
    c_diff = Num.absDiff a.c b.c
    r_diff = Num.absDiff a.r b.r

    r =
        if a.r < b.r then
            (Num.subWrap a.r r_diff, Num.addWrap b.r r_diff)
        else
            (Num.addWrap a.r r_diff, Num.subWrap b.r r_diff)

    c =
        if a.c < b.c then
            (Num.subWrap a.c c_diff, Num.addWrap b.c c_diff)
        else
            (Num.addWrap a.c c_diff, Num.subWrap b.c c_diff)

    ({ r: r.0, c: c.0 }, { r: r.1, c: c.1 })

expect
    a = anti_nodes ({ r: 3, c: 4 }, { r: 5, c: 5 })
    a == ({ c: 3, r: 1 }, { c: 6, r: 7 })

expect
    a = anti_nodes ({ r: 3, c: 4 }, { r: 4, c: 8 })
    a == ({ c: 0, r: 2 }, { c: 12, r: 5 })

expect
    a = anti_nodes ({ r: 1, c: 8 }, { r: 2, c: 5 })
    a == ({ r: 0, c: 11 }, { r: 3, c: 2 })

# select all the node pairs for a specific frequency
to_pairs : List { r : U64, c : U64 }, Set ({ r : U64, c : U64 }, { r : U64, c : U64 }) -> Set ({ r : U64, c : U64 }, { r : U64, c : U64 })
to_pairs = \nodes, acc ->
    when nodes is
        [] -> acc
        [first, .. as rest] ->
            to_pairs rest (List.walk rest acc \set, n -> Set.insert set (first, n))

expect
    a = to_pairs [{ c: 6, r: 5 }, { c: 8, r: 8 }, { c: 9, r: 9 }] (Set.empty {})
    a
    == Set.fromList [
        ({ c: 6, r: 5 }, { c: 8, r: 8 }),
        ({ c: 6, r: 5 }, { c: 9, r: 9 }),
        ({ c: 8, r: 8 }, { c: 9, r: 9 }),
    ]

parse_antennas : Str -> { maxR : U64, maxC : U64, ax : Dict U8 (List { r : U64, c : U64 }) }
parse_antennas = \input ->

    lines = input |> Str.splitOn "\n"

    maxR = List.len lines - 1

    maxC =
        List.first lines
        |> Result.map Str.toUtf8
        |> Result.map \bytes -> List.len bytes - 1
        |> unwrap

    ax =
        lines
        |> List.walkWithIndex (Dict.empty {}) \line_dict, line, r ->
            line
            |> Str.toUtf8
            |> List.walkWithIndex line_dict \dict, b, c ->
                if b == '.' then
                    dict
                else
                    Dict.update dict b \state ->
                        when state is
                            Err Missing -> Ok [{ r, c }]
                            Ok list -> Ok (List.append list { r, c })

    { maxR, maxC, ax }

expect
    a = parse_antennas example_input
    a
    == {
        maxR: 11,
        maxC: 11,
        ax: Dict.fromList [
            ('0', [{ c: 8, r: 1 }, { c: 5, r: 2 }, { c: 7, r: 3 }, { c: 4, r: 4 }]),
            ('A', [{ c: 6, r: 5 }, { c: 8, r: 8 }, { c: 9, r: 9 }]),
        ],
    }

example_input =
    """
    ............
    ........0...
    .....0......
    .......0....
    ....0.......
    ......A.....
    ............
    ............
    ........A...
    .........A..
    ............
    ............
    """

unwrap = \result ->
    when result is
        Ok value -> value
        Err _ -> crash "expected Ok"
