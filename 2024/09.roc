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

main = AoC.solve { year: 2024, day: 9, title: "Disk Fragmenter", part1, part2 }

part1 : Str -> Result Str [ParsingFailure Str, ParsingIncomplete Str]
part1 = \input ->

    dense = parse_dense_format input

    compacted = move_file_blocks dense 0 (List.len dense - 1)

    score =
        List.walkWithIndex compacted 0u64 \sum, block, index ->
            if block == e then
                sum
            else
                sum + (index * (Num.toU64 block))

    score |> Num.toStr |> Ok

expect part1 example_input == Ok "1928"

part2 : Str -> Result Str _
part2 = \_input ->
    Err TODO

# expect part2 example_input == Ok "123"

example_input = "2333133121414131402"

parse_dense_format : Str -> List U16
parse_dense_format = \input ->

    help = \bytes, next, blocks ->
        when bytes is
            [] -> blocks
            [first, .. as rest] ->
                (new_blocks, new_next) = append_blocks blocks next (first - '0')
                help rest new_next new_blocks

    input |> Str.trim |> Str.toUtf8 |> help (Block 0) []

expect
    a = parse_dense_format example_input
    a == [0, 0, e, e, e, 1, 1, 1, e, e, e, 2, e, e, e, 3, 3, 3, e, 4, 4, e, 5, 5, 5, 5, e, 6, 6, 6, 6, e, 7, 7, 7, e, 8, 8, 8, 8, 9, 9]

append_blocks : List U16, [Free U16, Block U16], U8 -> (List U16, [Free U16, Block U16])
append_blocks = \blocks, next, len ->
    when next is
        Free id ->
            new_blocks = List.range { start: At 0, end: Before len } |> List.map \_ -> e
            (List.concat blocks new_blocks, Block id)

        Block id ->
            new_blocks = List.range { start: At 0, end: Before len } |> List.map \_ -> id
            (List.concat blocks new_blocks, Free (id + 1))

expect
    a = append_blocks [] (Free 2) 2
    a == ([e, e], Block 2)

expect
    a = append_blocks [1, 1] (Block 2) 3
    a == ([1, 1, 2, 2, 2], Free 3)

move_file_blocks : List U16, U64, U64 -> List U16
move_file_blocks = \blocks, left_free, right_block ->
    when (shift_right blocks left_free, shift_left blocks right_block) is
        (Ok left, Ok right) if left < right -> move_file_blocks (List.swap blocks left right) left right
        _ -> blocks

expect
    blocks = [0, e, e, 1, 1, 1, e, e, e, e, 2, 2, 2, 2, 2]
    a = move_file_blocks blocks 0 (List.len blocks - 1)
    a == [0, 2, 2, 1, 1, 1, 2, 2, 2, e, e, e, e, e, e]

expect
    blocks = [0, 0, e, e, e, 1, 1, 1, e, e, e, 2, e, e, e, 3, 3, 3, e, 4, 4, e, 5, 5, 5, 5, e, 6, 6, 6, 6, e, 7, 7, 7, e, 8, 8, 8, 8, 9, 9]
    a = move_file_blocks blocks 0 (List.len blocks - 1)
    a == [0, 0, 9, 9, 8, 1, 1, 1, 8, 8, 8, 2, 7, 7, 7, 3, 3, 3, 6, 4, 4, 6, 5, 5, 5, 5, 6, 6, e, e, e, e, e, e, e, e, e, e, e, e, e, e]

# move right until index points at an empty block
shift_right : List U16, U64 -> Result U64 [NoEmptyBlocks]
shift_right = \blocks, i ->
    if List.get blocks i == Ok e then
        Ok i
    else if i < List.len blocks then
        shift_right blocks (i + 1)
    else
        Err NoEmptyBlocks

expect shift_right [1, 1, e, e] 0 == Ok 2
expect shift_right [1, 1, e, e] 2 == Ok 2
expect shift_right [1, 1, 1, 1] 2 == Err NoEmptyBlocks

# move left until index points at an full block
shift_left : List U16, U64 -> Result U64 [NoFullBlocks]
shift_left = \blocks, i ->
    if List.get blocks i != Ok e then
        Ok i
    else if i > 0 then
        shift_left blocks (i - 1)
    else
        Err NoFullBlocks

expect shift_left [1, 1, e, e] 3 == Ok 1
expect shift_left [1, 1, e, e] 1 == Ok 1
expect shift_left [e, e, e, e] 2 == Err NoFullBlocks

# empty block
e : U16
e = Num.maxU16
