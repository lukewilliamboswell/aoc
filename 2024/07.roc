app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.9.0/w8YKp2YAgQt5REYk912HfKAHBjcXsrnvtjI0CBzoAT4.tar.br",
    aoc: "https://github.com/lukewilliamboswell/aoc-template/releases/download/0.2.0/tlS1ZkwSKSB87_3poSOXcwHyySe0WxWOWQbPmp7rxBw.tar.br",
}

import pf.Stdin
import pf.Stdout
import pf.Utc
import parser.String exposing [parseStr, digits, string, codeunit]
import parser.Parser exposing [Parser, sepBy]
import aoc.AoC {
    stdin: Stdin.readToEnd,
    stdout: Stdout.write,
    time: \{} -> Utc.now {} |> Task.map Utc.toMillisSinceEpoch,
}

Test : { test_value : U64, test_inputs : List U64 }

Op : [Add, Mul, Con]

Ast : [
    Num U64,
    Add Ast Ast,
    Mul Ast Ast,
    Con Ast Ast,
]

main = AoC.solve { year: 2024, day: 7, title: "Bridge Repair", part1, part2 }

part1 : Str -> Result Str _
part1 = \input ->

    tests : List Test
    tests = parseStr? (sepBy parse_test (codeunit '\n')) (Str.trim input)

    valid_tests : List Test
    valid_tests =
        List.keepIf tests \test ->
            k = 2 # Add, Mul
            find_first_valid_ast test k |> Result.isOk

    valid_tests
    |> List.map .test_value
    |> List.sum
    |> Num.toStr
    |> Ok

expect
    actual = part1 example_input
    actual == Ok "3749"

part2 : Str -> Result Str _
part2 = \input ->

    tests : List Test
    tests = parseStr? (sepBy parse_test (codeunit '\n')) (Str.trim input)

    valid_tests : List Test
    valid_tests =
        List.keepIf tests \test ->
            k = 3 # Add, Mul, Concat ||
            find_first_valid_ast test k |> Result.isOk

    valid_tests
    |> List.map .test_value
    |> List.sum
    |> Num.toStr
    |> Ok

expect
    actual = part2 example_input
    actual == Ok "11387"

find_first_valid_ast : Test, U64 -> Result Ast {}
find_first_valid_ast = \{ test_value, test_inputs }, k ->

    n = List.len test_inputs - 1

    combinations : List (List Op)
    combinations = generate_combinations n k |> List.map \cs -> List.map cs from_code

    List.walkUntil combinations (Err {}) \state, cs ->
        ast = to_ast test_inputs cs

        if eval ast == test_value then
            Break (Ok ast)
        else
            Continue state

to_ast : List U64, List Op -> Ast
to_ast = \vals, ops ->
    when (vals, ops) is
        ([val], []) -> Num val
        ([.. as restVals, val], [.. as restOps, op]) ->
            when op is
                Add -> Add (Num val) (to_ast restVals restOps)
                Mul -> Mul (Num val) (to_ast restVals restOps)
                Con -> Con (Num val) (to_ast restVals restOps)

        _ -> crash "unexpected length of ops and vals"

expect to_ast [10, 20] [Add] == Add (Num 20) (Num 10)
expect to_ast [10, 20, 30] [Mul, Add] == Add (Num 30) (Mul (Num 20) (Num 10))
expect to_ast [10, 20] [Con] == Con (Num 20) (Num 10)
expect eval (to_ast [15, 6] [Con]) == 156

generate_combinations : U64, U64 -> List (List U64)
generate_combinations = \n, k ->

    help = \prefix, remaining ->
        if remaining == 0 then
            [prefix]
        else
            List.range { start: At 0, end: At (k - 1) }
            |> List.joinMap \digit ->
                help
                    (List.append prefix digit)
                    (remaining - 1)

    help [] n

expect generate_combinations 0 0 == [[]]
expect generate_combinations 2 2 == [[0, 0], [0, 1], [1, 0], [1, 1]]
expect generate_combinations 2 3 == [[0, 0], [0, 1], [0, 2], [1, 0], [1, 1], [1, 2], [2, 0], [2, 1], [2, 2]]
expect generate_combinations 3 2 == [[0, 0, 0], [0, 0, 1], [0, 1, 0], [0, 1, 1], [1, 0, 0], [1, 0, 1], [1, 1, 0], [1, 1, 1]]

eval : Ast -> U64
eval = \node ->
    when node is
        Num val -> val
        Add a b -> (eval a) + (eval b)
        Mul a b -> (eval a) * (eval b)
        Con a b -> concat_digits (eval a) (eval b)

expect eval (Num 10) == 10
expect eval (Add (Num 10) (Num 20)) == 30
expect eval (Mul (Add (Num 10) (Num 20)) (Num 2)) == 60

# srsly... what is this! who concat's digits !?
concat_digits : U64, U64 -> U64
concat_digits = \a, b ->
    aStr = Num.toStr a
    bStr = Num.toStr b

    when Str.concat bStr aStr |> Str.toU64 is
        Ok n -> n
        Err _ -> crash "concating digits is silly..."

from_code : U64 -> Op
from_code = \op_code ->
    if op_code == 0 then
        Add
    else if op_code == 1 then
        Mul
    else if op_code == 2 then
        Con
    else
        crash "unexpected op_code"

expect from_code 0 == Add
expect from_code 1 == Mul

parse_test : Parser _ Test
parse_test =
    { Parser.map2 <-
        test_value: digits,
        _: string ": ",
        test_inputs: sepBy digits (codeunit ' '),
    }

expect parseStr parse_test "190: 10 19" == Ok { test_value: 190, test_inputs: [10, 19] }
expect parseStr parse_test "21037: 9 7 18 13" == Ok { test_value: 21037, test_inputs: [9, 7, 18, 13] }

example_input =
    """
    190: 10 19
    3267: 81 40 27
    83: 17 5
    156: 15 6
    7290: 6 8 6 15
    161011: 16 10 13
    192: 17 8 14
    21037: 9 7 18 13
    292: 11 6 16 20
    """
