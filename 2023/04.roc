app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.16.0/O00IPk-Krg_diNS2dVWlI0ZQP794Vctxzv0ha96mK0E.tar.br",
    aoc: "https://github.com/lukewilliamboswell/aoc-template/releases/download/0.1.0/DcTQw_U67F22cX7pgx93AcHz_ShvHRaFIFjcijF3nz0.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.8.0/PCkJq9IGyIpMfwuW-9hjfXd6x-bHb1_OZdacogpBcPM.tar.br",
}

import pf.Stdin
import pf.Stdout
import pf.Utc
import aoc.AoC {
    stdin: Stdin.readToEnd,
    stdout: Stdout.write,
    time: \{} -> Utc.now {} |> Task.map Utc.toMillisSinceEpoch,
}
import parser.String exposing [string, digits, parseStr, codeunit]
import parser.Parser exposing [Parser, sepBy, const, keep, skip, chompWhile]

main =
    AoC.solve {
        year: 2023,
        day: 4,
        title: "Scratchcards",
        part1,
        part2,
    }

exampleInput =
    """
    Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
    Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
    Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
    Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
    Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
    Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
    """

part1 : Str -> Result Str _
part1 = \input ->

    cards = parseStr? (sepBy cardParser (codeunit '\n')) input

    sum = cards |> List.map scoreCard |> List.sum

    Ok "The scratch cards are worth a total of $(Num.toStr sum) points."

expect part1 exampleInput == Ok "The scratch cards are worth a total of 13 points."

part2 : Str -> Result Str _
part2 = \input ->

    cards = parseStr? (sepBy cardParser (codeunit '\n')) input

    cardsWithWins = cards |> List.map \card -> (card, countWins card)

    initCounts =
        List.range { start: At 0, end: Before (List.len cards) }
        |> List.map \_ -> 1 # initiliase to one to include starting scratchy

    counts = List.walk cardsWithWins initCounts countCards

    sum = counts |> List.sum

    Ok "The total number is $(Num.toStr sum) scratchcards."

expect part2 exampleInput == Ok "The total number is 30 scratchcards."

Card : { id : U64, winning : List U64, picks : List U64 }
CardCounts : List U64 # note card count index == (card.id - 1)

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

scoreCard : Card -> U64
scoreCard = \card -> card |> countWins |> calcScore 0

countWins : Card -> U64
countWins = \card -> countWinsHelp card.picks card.winning 0

countWinsHelp : List U64, List U64, U64 -> U64
countWinsHelp = \picks, winning, wins ->
    next = List.dropFirst picks 1

    when picks is
        [] -> wins # base case
        [p, .. ] ->
            if List.contains winning p then
                countWinsHelp next winning (wins + 1)
            else
                countWinsHelp next winning wins

calcScore : U64, U64 -> U64
calcScore = \wins, score ->
    if wins == 0 then
        score # base case
    else if score == 0 then
        calcScore (wins - 1) 1
    else
        calcScore (wins - 1) (score * 2)

expect scoreCard exampleCard1 == 8
expect scoreCard exampleCard2 == 2

countCards : CardCounts, (Card, U64) -> CardCounts
countCards = \counts, (card, wins) ->

    idx = card.id - 1

    currentCount =
        when List.get counts idx is
            Ok c -> c
            Err OutOfBounds -> crash "got invalid index for card counts"

    countCardsHelp counts wins currentCount card.id

countCardsHelp : CardCounts, U64, U64, U64 -> CardCounts
countCardsHelp = \counts, winsRemaining, currentCount, currentId ->
    nextId = currentId + 1

    if winsRemaining == 0 then
        counts
    else

        inc = \c -> c + currentCount
        idx = nextId - 1
        updatedCounts = List.update counts idx inc

        countCardsHelp updatedCounts (winsRemaining - 1) currentCount nextId
