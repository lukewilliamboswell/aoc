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

main = AoC.solve { year: 2024, day: 6, title: "Guard Gallivant", part1, part2 }

part1 : Str -> Result Str []
part1 = \input ->

    { obstructions , guard, maxR, maxC } = parseMap (Str.trim input)

    #dbg obstructions

    outside = \pos -> pos.r < 0 || pos.r > maxR || pos.c < 0 || pos.c > maxC

    walk = \steps, current ->
        next = stepGuard current obstructions

        if outside next.pos then
            steps
        else
            walk (List.append steps next.pos) next

    allSteps : List Position
    allSteps = walk [guard.pos] guard

    #dbg allSteps

    allSteps |> Set.fromList |> Set.len |> Num.toStr |> Ok

expect
    actual = part1 exampleInput
    actual == Ok "41"

part2 : Str -> Result Str _
part2 = \_input ->

    Ok ""

#expect
#    actual = part2 exampleInput
#    actual == Ok "143"

stepGuard : Guard, Set Position -> Guard
stepGuard = \{ facing, pos }, obstructions ->
    nextPos =
        when facing is
            Up -> { r: Num.subWrap pos.r 1, c: pos.c }
            Down -> { r: Num.addWrap pos.r 1, c: pos.c }
            Left -> { r: pos.r, c: Num.subWrap pos.c 1 }
            Right -> { r: pos.r, c: Num.addWrap pos.c 1 }

    if Set.contains obstructions nextPos then
        when facing is
            Up -> stepGuard { facing: Right, pos } obstructions
            Down -> stepGuard { facing: Left, pos } obstructions
            Left -> stepGuard { facing: Up, pos } obstructions
            Right -> stepGuard { facing: Down, pos } obstructions
    else
        { facing, pos: nextPos }

Position : { r : U64, c : U64 }
Guard : { facing : [Up, Down, Left, Right], pos : Position }

parseMap : Str -> { obstructions : Set Position, guard : Guard, maxR: U64, maxC: U64 }
parseMap = \input ->

    help : List U8, List Position, Guard, U64, U64, U64, U64 -> _
    help = \bytes, obs, guard, maxR, maxC, row, col ->
        when bytes is
            [] -> { obstructions: Set.fromList obs, guard, maxR, maxC }
            [next, .. as rest] if next == '\n' ->
                newWidth = row + 1
                help rest obs guard newWidth (maxC + 1) (row + 1) 0

            [next, .. as rest] if next == '.' ->
                help rest obs guard maxR maxC row (col + 1)

            [next, .. as rest] if next == '#' ->
                newObs = List.append obs { r: row, c: col }
                help rest newObs guard maxR maxC row (col + 1)

            [next, .. as rest] if next == '^' ->
                newGuard = { facing: Up, pos: { r: row, c: col } }
                help rest obs newGuard maxR maxC row (col + 1)

            _ -> crash "unexpected input"

    help (Str.toUtf8 input) (List.withCapacity 1000) { facing: Down, pos: { r: 0, c: 0 } } 0 0 0 0

exampleInput =
    """
    ....#.....
    .........#
    ..........
    ..#.......
    .......#..
    ..........
    .#..^.....
    ........#.
    #.........
    ......#...
    """

expect
    { obstructions, guard, maxR, maxC } = parseMap exampleInput
    guard
    == { facing: Up, pos: { r: 6, c: 4 } }
    &&
    obstructions
    == Set.fromList [
        { c: 4, r: 0 },
        { c: 9, r: 1 },
        { c: 2, r: 3 },
        { c: 7, r: 4 },
        { c: 1, r: 6 },
        { c: 8, r: 7 },
        { c: 0, r: 8 },
        { c: 6, r: 9 },
    ]
    &&
    maxR == 9
    &&
    maxC == 9
