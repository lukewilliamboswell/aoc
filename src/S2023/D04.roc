interface S2023.D04
    exposes [solution]
    imports [
        AoC,
        Parser.Core.{ Parser, const, keep, skip, sepBy, chompWhile },
        Parser.String.{ parseStr, string, digits, codeunit },
        "2023-04.txt" as puzzleInput : Str,
    ]

solution : AoC.Solution
solution = { year: 2023, day: 4, title: "src/S2023/D03.roc", part1, part2, puzzleInput }

exampleInput =
    """
    Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
    Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
    Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
    Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
    Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
    Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
    """

part1 : Str -> Result Str [NotImplemented, Error Str]
part1 = \input ->

    cards <-
        parseStr (sepBy cardParser (codeunit '\n')) input
        |> Result.mapErr \err -> 
            dbg err
            Error "unable to parse input"
        |> Result.try

    sum = 
        cards
        |> List.map scoreCard
        |> List.sum 

    Ok "The scratch cards are worth a total of \(Num.toStr sum) points."

expect part1 exampleInput == Ok "The scratch cards are worth a total of 13 points."

part2 : Str -> Result Str [NotImplemented, Error Str]
part2 = \_input -> Err NotImplemented

Card : { id : Nat, winning : List Nat, picks : List Nat }

cardParser : Parser (List U8) Card
cardParser =

    # we need to handle both single and double spaces before digits
    eatWhitespace = chompWhile \b -> b == ' '

    const (\id -> \winning -> \picks -> { id, winning, picks })
    |> skip (string "Card")
    |> skip (eatWhitespace)
    |> keep (digits)
    |> skip (string ":")
    |> skip (eatWhitespace)
    |> keep (sepBy digits eatWhitespace)
    |> skip (string " |")
    |> skip (eatWhitespace)
    |> keep (sepBy digits eatWhitespace) 

exampleCard1 = {
    id: 1,
    winning: [41, 48, 83, 86, 17],
    picks: [83, 86, 6, 31, 17, 9, 48, 53],
}

exampleCard2 = {
    id: 2,
    winning: [13, 32, 20, 16, 61],
    picks: [61, 30, 68, 82, 17, 32, 24, 19],
}

expect parseStr cardParser "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53" == Ok exampleCard1
expect parseStr cardParser "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19" == Ok exampleCard2

scoreCard : Card -> Nat
scoreCard = \{winning, picks} -> scoreCardHelp picks winning 1 0

scoreCardHelp : List Nat, List Nat, Nat, Nat -> Nat
scoreCardHelp = \picks, winning, multiplier, score ->
    next = List.dropFirst picks 1
    nextMultiplier = multiplier + 1

    when picks is
        [] -> 
            score
        [first, .. as rest] -> 
            if List.contains winning first then
                if score == 0 then
                    scoreCardHelp next winning nextMultiplier 1
                else 
                    scoreCardHelp next winning nextMultiplier (2 * score)
            else 
                scoreCardHelp next winning multiplier score

expect scoreCard exampleCard1 == 8
expect scoreCard exampleCard2 == 2