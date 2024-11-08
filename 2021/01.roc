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
        year: 2021,
        day: 1,
        title: "Sonar Sweep",
        part1,
        part2,
    }

part1 : Str -> Result Str Str
part1 = \input ->
    input
    |> parseInput
    |> countDepthIncreases
    |> Num.toStr
    |> \answer -> Ok "The number of depth increases is $(answer)"

part2 : Str -> Result Str Str
part2 = \input ->
    input
    |> parseInput
    |> slidingWindow
    |> countDepthIncreases
    |> Num.toStr
    |> \answer -> Ok "The number of depth increases is $(answer)"

parseInput : Str -> List U64
parseInput = \content ->
    content
    |> Str.split "\n"
    |> List.keepOks Str.toU64

expect parseInput "not-a-number\n123\n345\n678\n" == [123, 345, 678]

countDepthIncreases : List U64 -> U64
countDepthIncreases = \depths ->
    depths
    |> List.walk
        { last: 0, count: 0 }
        \state, depth ->
            next = { state & last: depth }

            when state.last is
                0 -> next
                l if l < depth -> { next & count: state.count + 1 }
                _ -> next
    |> .count

expect countDepthIncreases [] == 0
expect countDepthIncreases [10, 15, 10] == 1
expect countDepthIncreases [199, 200, 208, 210, 200, 207, 240, 269, 260, 263] == 7

slidingWindow : List U64 -> List U64
slidingWindow = \depths ->
    depths
    |> List.walk
        { n1: Nothing, n2: Nothing, filtered: [] }
        \state, depth ->
            when Pair state.n1 state.n2 is
                Pair Nothing Nothing -> { n1: Just depth, n2: Nothing, filtered: [] }
                Pair (Just n1) Nothing -> { n1: Just depth, n2: Just n1, filtered: [] }
                Pair (Just n1) (Just n2) ->
                    { n1: Just depth, n2: Just n1, filtered: List.append state.filtered (n1 + n2 + depth) }

                Pair _ _ -> state
    |> .filtered

expect slidingWindow [1, 2, 3, 4, 5, 6] == [6, 9, 12, 15]
expect slidingWindow [199, 200, 208, 210, 200, 207, 240, 269, 260, 263] == [607, 618, 618, 617, 647, 716, 769, 792]
