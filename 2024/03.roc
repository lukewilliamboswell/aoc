app [main!] {
	pf: platform "../../basic-cli/platform/main.roc",
	parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/1.0.2/FrnJ4RGDKpQyoDyESNoBwFNviY4ZGbMVLnUjW9tvSRjk.tar.zst",
}

import pf.OsStr
import pf.Stdin
import pf.Stdout
import parser.Parser exposing [Parser]
import parser.String

Op : [Mul(U64, U64), Do, Dont]

main! : List(OsStr) => Try({}, _)
main! = |_| {
	input = Str.from_utf8(Stdin.read_to_end!()?) ? |err| InvalidUtf8(err)
	Stdout.line!("Part 1: ${part1(input)}")?
	Stdout.line!("Part 2: ${part2(input)}")?
	Ok({})
}

part1 : Str -> Str
part1 = |input|
	parse(input.to_utf8(), []).map(
		|op| match op {
			Mul(a, b) => a * b
			Do | Dont => 0
		},
	).sum().to_str()

## Part one adds every valid multiplication instruction.
expect part1(example_input_part1) == "161"

part2 : Str -> Str
part2 = |input| evaluate(parse(input.to_utf8(), []), Bool.True, 0).to_str()

## Part two honours do and don't instructions.
expect part2(example_input_part2) == "48"

parse : List(U8), List(Op) -> List(Op)
parse = |input, result| match input {
	[] => result
	[_, .. as rest] => match Parser.parse_partial(parse_op, input) {
		Ok({ val: op, input: remaining }) => parse(remaining, result.append(op))
		Err(_) => parse(rest, result)
	}
}

parse_mul : Parser(String.Utf8, Op)
parse_mul = Parser.const(|a| |b| Mul(a, b))
	.skip(String.string("mul("))
	.keep(String.digits)
	.skip(String.codeunit(','))
	.keep(String.digits)
	.skip(String.codeunit(')'))

parse_op : Parser(String.Utf8, Op)
parse_op = String.one_of([
	parse_mul,
	Parser.const(Dont).skip(String.string("don't()")),
	Parser.const(Do).skip(String.string("do()")),
])

## Multiplication parsing reads both operands.
expect String.parse_str(parse_mul, "mul(223,445)") == Ok(Mul(223, 445))

## Malformed multiplication instructions are rejected.
expect String.parse_str(parse_mul, "mul[3,7]").is_err()

## Scanning ignores noise while retaining valid operations in order.
expect parse(example_input_part2.to_utf8(), []) == [Mul(2, 4), Dont, Mul(5, 5), Mul(11, 8), Do, Mul(8, 5)]

evaluate : List(Op), Bool, U64 -> U64
evaluate = |operations, enabled, total| match operations {
	[] => total
	[Mul(a, b), .. as rest] if enabled => evaluate(rest, enabled, total + a * b)
	[Mul(_, _), .. as rest] => evaluate(rest, enabled, total)
	[Do, .. as rest] => evaluate(rest, Bool.True, total)
	[Dont, .. as rest] => evaluate(rest, Bool.False, total)
}

## Evaluation skips multiplication while disabled.
expect evaluate([Mul(2, 4), Dont, Mul(5, 5), Do, Mul(8, 5)], Bool.True, 0) == 48

example_input_part1 = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

example_input_part2 = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
