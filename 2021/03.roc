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

main =
    AoC.solve {
        year: 2021,
        day: 3,
        title: "Binary Diagnostic",
        part1,
        part2,
    }

part1 : Str -> Result Str Str
part1 = \input ->

    numbers = parseInput input

    g = numbers |> countBits |> compareBitCounts Gamma
    e = numbers |> countBits |> compareBitCounts Epilson
    p = Num.toStr ((binaryToDecimal g) * (binaryToDecimal e))

    Ok "Part 1 -- The gamma:$(g), epsilon:$(e), power:$(p)"

part2 : Str -> Result Str Str
part2 = \input ->

    numbers = parseInput input

    g = numbers |> countBits |> compareBitCounts Gamma
    e = numbers |> countBits |> compareBitCounts Epilson
    p = Num.toStr ((binaryToDecimal g) * (binaryToDecimal e))

    Ok "Part 1 -- The gamma:$(g), epsilon:$(e), power:$(p)"


binaryToDecimal : Str -> U128
binaryToDecimal = \x ->
    x
    |> Str.toU128
    |> Result.withDefault 0

expect binaryToDecimal "0b010111011111" == 1503
expect binaryToDecimal "0b101000100000" == 2592

parseInput : Str -> List [One U64, Zero U64]
parseInput = \contents ->
    contents
    |> Str.splitOn "\n"
    |> List.map lineBitWithIndex
    |> List.join

# Takes a string representation of a binary number and converts into a list of One and Zero Bits
lineBitWithIndex : Str -> List [One U64, Zero U64]
lineBitWithIndex = \binaryNumber ->
    binaryNumber
    |> Str.toUtf8
    |> List.mapWithIndex
        (\scalar, i ->
            if scalar == '0' then
                Ok (Zero i)
            else if scalar == '1' then
                Ok (One i)
            else
                Err ""
        )
    |> List.keepOks (\x -> x)

expect Str.toUtf8 "0011001" == ['0', '0', '1', '1', '0', '0', '1']
expect lineBitWithIndex "001" == [Zero 0, Zero 1, One 2]
expect ("001\n010" |> Str.splitOn "\n" |> List.map lineBitWithIndex |> List.join) == [Zero 0, Zero 1, One 2, Zero 0, One 1, Zero 2]

# Counts the Bits for each index position
countBits : List [One U64, Zero U64] -> Dict U64 { zeroCount : U64, oneCount : U64 }
countBits = \bits ->
    List.walk bits (Dict.empty {}) \state, elem ->
        updateCounts state elem

# Compare Bit counts and return binary string representation
compareBitCounts : Dict U64 { zeroCount : U64, oneCount : U64 }, [Gamma, Epilson] -> Str
compareBitCounts = \dict, policy ->
    (when policy is
        Gamma ->
            Dict.walk
                dict
                ""
                (\binaryStr, _, counts ->
                    if counts.zeroCount < counts.oneCount then
                        Str.concat binaryStr "1"
                    else
                        Str.concat binaryStr "0"
                )
        Epilson ->
            Dict.walk
                dict
                ""
                (\binaryStr, _, counts ->
                    if counts.zeroCount < counts.oneCount then
                        Str.concat binaryStr "0"
                    else
                        Str.concat binaryStr "1"
                )
    )
    |> \x -> Str.concat "0b" x

testDict = (Dict.empty {})
    |> updateCounts (Zero 0)
    |> updateCounts (Zero 0)
    |> updateCounts (One 0)
    |> updateCounts (One 1)
    |> updateCounts (One 1)
    |> updateCounts (Zero 1)
    |> updateCounts (One 2)
    |> updateCounts (Zero 2)
    |> updateCounts (Zero 2)

expect compareBitCounts testDict Gamma == "0b010"
expect compareBitCounts testDict Epilson == "0b101"

# Updates our bit count state
updateCounts : Dict U64 { zeroCount : U64, oneCount : U64 }, [One U64, Zero U64] -> Dict U64 { zeroCount : U64, oneCount : U64 }
updateCounts = \dict, bit ->
    when bit is
        One i ->
            currentCount = Dict.get dict i
            when currentCount is
                Ok {zeroCount, oneCount} -> Dict.insert dict i {zeroCount, oneCount : oneCount + 1}
                Err _ -> Dict.insert dict i {zeroCount : 0, oneCount : 1}
        Zero i ->
            currentCount = Dict.get dict i
            when currentCount is
                Ok {zeroCount, oneCount} -> Dict.insert dict i {zeroCount : zeroCount + 1, oneCount}
                Err _ -> Dict.insert dict i {zeroCount : 1, oneCount : 0}

expect (emptyDict |> updateCounts (One 6)) == (Dict.empty {} |> Dict.insert 6 { zeroCount: 0, oneCount: 1 })

emptyDict = Dict.empty {}
