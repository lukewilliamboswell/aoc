interface S2023.D10
    exposes [solution]
    imports [
        AoC,
        # Parser.Core.{ Parser, const, oneOf, keep, skip, sepBy },
        # Parser.String.{ parseStr, digits, codeunit },
        "2023-10.txt" as puzzleInput : Str,
    ]

solution : AoC.Solution
solution = { year: 2023, day: 10, title: "Pipe Maze", part1, part2, puzzleInput }

part1 : Str -> Result Str [NotImplemented, Error Str]
part1 = \_ -> Err NotImplemented

# expect
#     res = part1 exampleInput
#     res == Ok "The the sum of the LAST extrapolated values 114"

part2 : Str -> Result Str [NotImplemented, Error Str]
part2 = \_ -> Err NotImplemented

# expect part2 exampleInput == Ok "The the sum of the FIRST extrapolated values 2"

exampleInput =
    """
    7-F7-
    .FJ|7
    SJLL7
    |F--J
    LJ.LJ
    """

Maze : List U8
Position : {row : U8, col : U8}
Pipe : [NorthSouth, EastWest, NorthEast, NorthWest, SouthEast, SouthWest, Start, Ground]

toIndx : Size, Position -> Nat

fromIdx : Size, Nat -> Position

get : Maze, Position -> Pipe
get = \maze, pos ->
    idx = toIndx pos

    when List.get maze pos is 
        Ok '.' -> Ground
        Ok '|' -> NorthSouth
        Ok '-' -> EastWest
        Ok 'L' -> NorthEast
        Ok 'J' -> NorthWest
        Ok '7' -> SouthWest
        Ok 'F' -> SouthEast
        Ok 'S' -> Start
        _ -> crash "unexpected input byte"

findStart : Maze -> Position
findStart = \maze ->
    List.walkWithIndex maze (0,0) 

# Position : {row : U8, col : U8}
# Pipe : [NorthSouth, EastWest, NorthEast, NorthWest, SouthEast, SouthWest, Start, Ground]
# Maze : {
#     start : Position,
#     pipes : List {pos : Position, pipe : Pipe},
# }

# expect parse exampleInput |> .start == {row: 2, col: 0} 


# parse : Str -> Maze
# parse = \input -> 
#     pipePipes = 
#         rowStr, row <- input |> Str.split "\n" |> List.mapWithIndex 
#         byte, col <- rowStr |> Str.toUtf8 |> List.mapWithIndex

#         pos = {row: Num.toU8 row, col: Num.toU8 col}

#         when byte is 
#             '.' -> {pos, pipe: Ground}
#             '|' -> {pos, pipe: NorthSouth}
#             '-' -> {pos, pipe: EastWest}
#             'L' -> {pos, pipe: NorthEast}
#             'J' -> {pos, pipe: NorthWest}
#             '7' -> {pos, pipe: SouthWest}
#             'F' -> {pos, pipe: SouthEast}
#             'S' -> {pos, pipe: Start}
#             _ -> crash "unexpected input byte"

#     pipes = pipePipes |> List.join

#     start =
#         pipes
#         |> List.keepIf \{pipe} -> pipe == Start
#         |> List.first
#         |> unwrap "couldn't find start pipe"
#         |> .pos

#     {start, pipes}

# unwrap = \thing, msg ->
#     when thing is
#         Ok unwrapped -> unwrapped
#         Err _ -> crash msg
    