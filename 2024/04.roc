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

main = AoC.solve { year: 2024, day: 4, title: "Ceres Search", part1, part2 }

XMAS : [X, M, A, S]

u8toXMAS : U8 -> XMAS
u8toXMAS = \b ->
    when b is
        'X' -> X
        'M' -> M
        'A' -> A
        'S' -> S
        _ -> crash "invalid XMAS"

expect u8toXMAS 'X' == X
expect u8toXMAS 'M' == M
expect u8toXMAS 'A' == A
expect u8toXMAS 'S' == S

part1 : Str -> Result Str _
part1 = \input ->

    array : Array2D [X, M, A, S]
    array = toArray2D input u8toXMAS

    words : List (List { r : U64, c : U64 })
    words =
        Dict.walk array (List.withCapacity 1000) \acc, { r, c }, _ ->
            allDirections =
                [U, D, L, R, UL, DL, DR, UR]
                |> List.map \direction -> getIdxInDirection { r, c } direction [{ r, c }] 3
                |> List.keepOks \idxs ->
                    when List.keepOks idxs \idx -> Dict.get array idx is
                        [x, m, a, s] if x == X && m == M && a == A && s == S -> Ok idxs
                        [s, a, m, x] if x == X && m == M && a == A && s == S -> Ok (List.reverse idxs)
                        _ -> Err NotXMAS

            List.concat acc allDirections

    filtered : Set (List { r : U64, c : U64 })
    filtered = Set.fromList words

    Set.len filtered
    |> Num.toStr
    |> Ok

part2 : Str -> Result Str []
part2 = \input ->

    array : Array2D [X, M, A, S]
    array = toArray2D input u8toXMAS

    words : List (List { r : U64, c : U64 })
    words =
        Dict.walk array (List.withCapacity 1000) \acc, middle, _ ->

            result =
                getXmasIdxs middle
                |> \idxs ->
                    when List.keepOks idxs \idx -> Dict.get array idx is
                        # M . M
                        # . A .
                        # S . S
                        [ul, ur, m, dl, dr] if ul == M && ur == M && m == A && dl == S && dr == S -> Ok idxs
                        # M . S
                        # . A .
                        # M . S
                        [ul, ur, m, dl, dr] if ul == M && ur == S && m == A && dl == M && dr == S -> Ok idxs
                        # S . S
                        # . A .
                        # M . M
                        [ul, ur, m, dl, dr] if ul == S && ur == S && m == A && dl == M && dr == M -> Ok idxs
                        # S . M
                        # . A .
                        # S . M
                        [ul, ur, m, dl, dr] if ul == S && ur == M && m == A && dl == S && dr == M -> Ok idxs
                        _ -> Err NotXMAS

            when result is
                Ok idxs -> List.concat acc [idxs]
                Err _ -> acc

    filtered : Set (List { r : U64, c : U64 })
    filtered = Set.fromList words

    Set.len filtered
    |> Num.toStr
    |> Ok

exampleInput =
    """
    MMMSXXMASM
    MSAMXMSMSA
    AMXSXMAAMM
    MSAMASMSMX
    XMASAMXAMM
    XXAMMXXAMA
    SMSMSASXSS
    SAXAMASAAA
    MAMMMXMMMM
    MXMXAXMASX
    """

expect
    result = part1 exampleInput
    result == Ok "18"

expect
    result = part2 exampleInput
    result == Ok "9"

Array2D a : Dict { r : U64, c : U64 } a

toArray2D : Str, (U8 -> a) -> Array2D a
toArray2D = \input, f ->
    List.mapWithIndex (Str.splitOn input "\n") \row, r ->
        List.mapWithIndex (Str.toUtf8 row) \b, c -> { r, c, v: f b }
    |> List.join
    |> List.walk (Dict.empty {}) \acc, { r, c, v } -> Dict.insert acc { r, c } v

calcRowCol : U64 -> (U64 -> { r : U64, c : U64 })
calcRowCol = \rowLen -> \i -> { r: i // rowLen, c: i % rowLen }

expect (calcRowCol 10) 0 == { r: 0, c: 0 }
expect (calcRowCol 10) 1 == { r: 0, c: 1 }
expect (calcRowCol 10) 9 == { r: 0, c: 9 }
expect (calcRowCol 10) 10 == { r: 1, c: 0 }

# ->> increasing Column direction
# |
# |
# v increasing Row direction
getIdxInDirection : { r : U64, c : U64 }, [U, D, L, R, UL, DL, DR, UR], List { r : U64, c : U64 }, U64 -> List { r : U64, c : U64 }
getIdxInDirection = \current, direction, acc, stepsRemaining ->
    if stepsRemaining == 0 then
        acc
    else
        next =
            when direction is
                U -> { r: Num.subWrap current.r 1, c: current.c }
                D -> { r: Num.addWrap current.r 1, c: current.c }
                L -> { r: current.r, c: Num.subWrap current.c 1 }
                R -> { r: current.r, c: Num.addWrap current.c 1 }
                UR -> { r: Num.subWrap current.r 1, c: Num.addWrap current.c 1 }
                DR -> { r: Num.addWrap current.r 1, c: Num.addWrap current.c 1 }
                DL -> { r: Num.addWrap current.r 1, c: Num.subWrap current.c 1 }
                UL -> { r: Num.subWrap current.r 1, c: Num.subWrap current.c 1 }

        getIdxInDirection next direction (List.append acc next) (stepsRemaining - 1)

expect
    actual = getIdxInDirection { r: 10, c: 10 } U [{ r: 10, c: 10 }] 3
    actual == [{ c: 10, r: 10 }, { c: 10, r: 9 }, { c: 10, r: 8 }, { c: 10, r: 7 }]

# UL, UR, M, DL, DR
getXmasIdxs : { r : U64, c : U64 } -> List { r : U64, c : U64 }
getXmasIdxs = \middle ->
    []
    |> List.concat (getIdxInDirection middle UL [] 1)
    |> List.concat (getIdxInDirection middle UR [] 1)
    |> List.concat [middle]
    |> List.concat (getIdxInDirection middle DL [] 1)
    |> List.concat (getIdxInDirection middle DR [] 1)
