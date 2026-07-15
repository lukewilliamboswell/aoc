app [main!] {
	pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.21.0-rc3/Cv3oh6aPxqZxDeh1dgnXPjZnYsoqQMvEawa6TUc71sxJ.tar.zst",
}

import pf.OsStr
import pf.Stdin
import pf.Stdout

Instruction : [Noop, AddX(I64)]

main! : List(OsStr) => Try({}, _)
main! = |_| {
	bytes = Stdin.read_to_end!()?
	input = Str.from_utf8(bytes) ? |err| InvalidUtf8(err)
	answer1 = part1(input)?
	answer2 = part2(input)?
	Stdout.line!("Part 1: ${answer1}")?
	Stdout.line!("Part 2:\n${answer2}")?
	Ok({})
}

part1 : Str -> Try(Str, _)
part1 = |input| {
	instructions = parse(input)?
	history = execute(instructions, 1, [])
	strength = signal_strength(history, [20, 60, 100, 140, 180, 220], 0)?
	Ok(strength.to_str())
}

part2 : Str -> Try(Str, _)
part2 = |input| {
	instructions = parse(input)?
	history = execute(instructions, 1, [])
	Ok(render(history, 0, [], ""))
}

parse : Str -> Try(List(Instruction), _)
parse = |input| parse_lines(input.trim().split_on("\n"), [])

parse_lines : List(Str), List(Instruction) -> Try(List(Instruction), _)
parse_lines = |lines, instructions|
	match lines {
		[] => Ok(instructions)
		[line, .. as rest] => {
			instruction = parse_instruction(line)?
			parse_lines(rest, instructions.append(instruction))
		}
	}

parse_instruction : Str -> Try(Instruction, _)
parse_instruction = |line|
	match line.split_on(" ") {
		["noop"] => Ok(Noop)
		["addx", value] => Ok(AddX(I64.from_str(value) ? |_| InvalidInstruction(line)))
		_ => Err(InvalidInstruction(line))
	}

execute : List(Instruction), I64, List(I64) -> List(I64)
execute = |instructions, register, history|
	match instructions {
		[] => history
		[Noop, .. as rest] => execute(rest, register, history.append(register))
		[AddX(value), .. as rest] => execute(rest, register + value, history.append(register).append(register))
	}

signal_strength : List(I64), List(U64), I64 -> Try(I64, _)
signal_strength = |history, cycles, total|
	match cycles {
		[] => Ok(total)
		[cycle, .. as rest] => {
			register = history.get(cycle - 1) ? |_| MissingCycle(cycle)
			signal_strength(history, rest, total + register * cycle.to_i64_wrap())
		}
	}

render : List(I64), U64, List(Str), Str -> Str
render = |history, index, rows, row| {
	match history {
		[] => Str.join_with(
			if row.is_empty() {
				rows
			} else {
				rows.append(row)
			},
			"\n",
		)
		[register, .. as rest] => {
			position = (index % 40).to_i64_wrap()
			pixel = if position >= register - 1 and position <= register + 1 {
				"#"
			} else {
				"."
			}
			updated_row = row.concat(pixel)
			if index % 40 == 39 {
				render(rest, index + 1, rows.append(updated_row), "")
			} else {
				render(rest, index + 1, rows, updated_row)
			}
		}
	}
}

## Instructions consume one or two cycles before updating the register.
expect execute([Noop, AddX(3), AddX(-5)], 1, []) == [1, 1, 1, 4, 4]

## The parser accepts both supported instruction forms.
expect parse("noop\naddx -3") == Ok([Noop, AddX(-3)])

## Rendering lights pixels covered by the three-wide sprite.
expect render([1, 1, 1, 4, 4], 0, [], "") == "#####"
