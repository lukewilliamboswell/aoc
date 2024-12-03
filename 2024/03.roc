app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.9.0/w8YKp2YAgQt5REYk912HfKAHBjcXsrnvtjI0CBzoAT4.tar.br",
    aoc: "https://github.com/lukewilliamboswell/aoc-template/releases/download/0.2.0/tlS1ZkwSKSB87_3poSOXcwHyySe0WxWOWQbPmp7rxBw.tar.br",
}

import pf.Stdin
import pf.Stdout
import pf.Utc
import parser.String exposing [parseStr, digits, string, oneOf]
import parser.Parser exposing [Parser, skip]
import aoc.AoC {
    stdin: Stdin.readToEnd,
    stdout: Stdout.write,
    time: \{} -> Utc.now {} |> Task.map Utc.toMillisSinceEpoch,
}

Op : [Mul U64 U64, Do, Dont]

main = AoC.solve { year: 2024, day: 3, title: "Mull It Over", part1, part2 }

part1 : Str -> Result Str []
part1 = \input ->
    parse (Str.toUtf8 input) []
    |> List.map \op ->
        when op is
            Mul a b -> a * b
            Do -> 0
            Dont -> 0
    |> List.sum
    |> Num.toStr
    |> Ok

exampleInputPart1 = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

expect part1 exampleInputPart1 == Ok "161"

part2 : Str -> Result Str []
part2 = \input ->
    parse (Str.toUtf8 input) []
    |> eval Bool.true 0
    |> Num.toStr
    |> Ok

exampleInputPart2 = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
exampleOps = [Mul 2 4, Dont, Mul 5 5, Mul 11 8, Do, Mul 8 5]

expect parse (Str.toUtf8 exampleInputPart2) [] == exampleOps
expect eval exampleOps Bool.true 0 == 48
expect part2 exampleInputPart2 == Ok "48"

parse : List U8, List Op -> List Op
parse = \input, acc ->
    when input is
        [] -> acc
        [next, .. as rest] if next == 'm' ->
            when Parser.parsePartial parseMul input is
                Ok { val: op, input: remaining } -> parse remaining (List.append acc op)
                Err _ -> parse rest acc

        [next, .. as rest] if next == 'd' ->
            when Parser.parsePartial parseDoNt input is
                Ok { val: op, input: remaining } -> parse remaining (List.append acc op)
                Err _ -> parse rest acc

        [_, .. as rest] -> parse rest acc

parseMul : Parser _ Op
parseMul =
    { Parser.map2 <-
        _: string "mul(",
        a: digits,
        _: string ",",
        b: digits,
        _: string ")",
    }
    |> Parser.map \{ a, b } -> Mul a b

expect parseStr parseMul "mul(2,4)" == Ok (Mul 2 4)
expect parseStr parseMul "mul(223,445)" == Ok (Mul 223 445)
expect parseStr parseMul "mul[3,7]" |> Result.isErr

parseDoNt : Parser _ Op
parseDoNt =
    oneOf [
        Parser.const Dont |> skip (string "don't()"),
        Parser.const Do |> skip (string "do()"),
    ]

expect parseStr parseDoNt "do()" == Ok Do
expect parseStr parseDoNt "don't()" == Ok Dont

eval : List Op, Bool, U64 -> U64
eval = \ops, do, acc ->
    when ops is
        [] -> acc
        [op, .. as rest] ->
            when op is
                Mul a b ->
                    if do then
                        eval rest do (acc + (a * b))
                    else
                        eval rest do acc

                Do -> eval rest Bool.true acc
                Dont -> eval rest Bool.false acc
