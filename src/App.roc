interface App
    exposes [
        solutions,
        solvePuzzle,
    ]
    imports [
        AoC,
        S2022.D01,
        S2022.D02,
        # S2022.D03,
        S2023.D01,
        S2023.D02,
        S2023.D03,
        S2023.D04,
        S2023.D05,
        S2023.D09,
        S2023.D10,
    ]

## Export a list of the solutions included in this app
solutions : List AoC.Solution
solutions = 
    [
        # 2022
        S2022.D01.solution,
        S2022.D02.solution,

        # 2023
        S2023.D01.solution,
        S2023.D02.solution,
        S2023.D03.solution,
        S2023.D04.solution,
        S2023.D05.solution,
        S2023.D09.solution,
        S2023.D10.solution,
    ]
    |> List.sortWith sortByYearAndDay

solvePuzzle : { year : U64, day : U64, puzzle : [Part1, Part2] } -> Result Str [NotImplemented, Error Str]
solvePuzzle = \selection ->

    result = solutions |> List.keepOks (AoC.filterByYearDay selection.year selection.day) |> List.first

    when (selection.puzzle, result) is
        (Part1, Ok solution) -> solution.part1 solution.puzzleInput
        (Part2, Ok solution) -> solution.part2 solution.puzzleInput
        (_, Err ListWasEmpty) -> Err NotImplemented

sortByYearAndDay : AoC.Solution, AoC.Solution -> [LT, EQ, GT]
sortByYearAndDay = \first, second -> 
    if first.year < second.year then 
        GT
    else if first.year > second.year then 
        LT
    else if first.day < second.day then 
        GT
    else if first.day > second.day then 
        LT
    else 
        EQ