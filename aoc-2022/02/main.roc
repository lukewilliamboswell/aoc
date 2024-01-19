app "aoc"
    packages { 
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.6.0/QOQW08n38nHHrVVkJNiPIjzjvbR3iMjXeFY5w1aT46w.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.2.0/dJQSsSmorujhiPNIvJKlQoI92RFIG_JQwUfIxZsCSwE.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Stderr,
        parser.Core.{ Parser, parse, const, keep, oneOf,map, oneOrMore, buildPrimitiveParser },
        "./input-day-2.txt" as fileContents : List U8,
    ]
    provides [main] to pf

main =

    input = fileContents |> List.append '\n'

    parser = oneOrMore rockPaperScissorParser
    answer =
        rounds <- parse parser input List.isEmpty |> Result.map 

        ts = totalScore rounds |> Num.toStr

        "The total score following guide \(ts) "

    when answer is
        Ok msg -> Stdout.line msg
        Err (ParsingFailure _) -> Stderr.line "Parsing failure"
        Err (ParsingIncomplete leftover) ->
            
            ls = leftover |> Str.fromUtf8 |> Result.withDefault ""

            Stderr.line "Parsing incomplete \(ls)"

RSP : [Rock, Scissor, Paper]
Outcome : [Loss, Draw, Win]

totalScore : List { opponent : RSP, guide : Outcome } -> U64
totalScore = \rounds ->
    rounds
    |> List.map \round ->
        round
        |> determineChoice
        |> calculateScore
    |> List.sum

calculateScore : { opponent : RSP, choice : RSP } -> U64
calculateScore = \{ opponent, choice } ->
    baseScore =
        when choice is
            Rock -> 1
            Paper -> 2
            Scissor -> 3

    winLossDrawScore =
        when determineOutcome { opponent, choice } is
            Loss -> 0
            Draw -> 3
            Win -> 6

    baseScore + winLossDrawScore

determineOutcome : { opponent : RSP, choice : RSP } -> Outcome
determineOutcome = \{ opponent, choice } ->
    when P opponent choice is
        P Rock Rock -> Draw
        P Rock Paper -> Win
        P Rock Scissor -> Loss
        P Paper Rock -> Loss
        P Paper Paper -> Draw
        P Paper Scissor -> Win
        P Scissor Rock -> Win
        P Scissor Paper -> Loss
        P Scissor Scissor -> Draw

determineChoice : { opponent : RSP, guide : Outcome } -> { opponent : RSP, choice : RSP }
determineChoice = \{ opponent, guide } ->
    if determineOutcome { opponent, choice: Rock } == guide then
        { opponent, choice: Rock }
    else if determineOutcome { opponent, choice: Paper } == guide then
        { opponent, choice: Paper }
    else
        { opponent, choice: Scissor }

rockPaperScissorParser : Parser (List U8) { opponent : RSP, guide : Outcome }
rockPaperScissorParser =
    const (\opponent -> \_ -> \guide -> \_ -> { opponent, guide })
    |> keep opponentParser
    |> keep parseBlankSpace
    |> keep guideParser
    |> keep parseNewLine

parseBlankSpace = parseUtf8 ' '
parseNewLine = parseUtf8 '\n'
opponentParser =
    oneOf [
        parseUtf8 'A' |> map \_ -> Rock,
        parseUtf8 'B' |> map \_ -> Paper,
        parseUtf8 'C' |> map \_ -> Scissor,
    ]

guideParser =
    oneOf [
        parseUtf8 'X' |> map \_ -> Loss,
        parseUtf8 'Y' |> map \_ -> Draw,
        parseUtf8 'Z' |> map \_ -> Win,
    ]

expect
    input = ['\n']
    parser = parseNewLine
    result = parse parser input List.isEmpty

    result == Ok '\n'

parseUtf8 : U8 -> Parser (List U8) U8
parseUtf8 = \x ->
    input <- buildPrimitiveParser
    
    when List.first input is
        Ok value ->
            if x == value then
                Ok { val: x, input: List.dropFirst input 1 }
            else
                Err (ParsingFailure "")

        Err ListWasEmpty ->
            Err (ParsingFailure "empty list")
