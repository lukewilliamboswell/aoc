app [main!] {
	pf: platform "../../basic-cli/platform/main.roc",
}

import pf.OsStr
import pf.Stdin
import pf.Stdout

Operation : [Add(U64), Multiply(U64), Square]

Monkey : {
	items : List(U64),
	operation : Operation,
	divisor : U64,
	if_true : U64,
	if_false : U64,
	inspections : U64,
}

main! : List(OsStr) => Try({}, _)
main! = |_| {
	bytes = Stdin.read_to_end!()?
	input = Str.from_utf8(bytes) ? |err| InvalidUtf8(err)
	answer = part1(input)?
	Stdout.line!("Part 1: ${answer}")?
	Ok({})
}

part1 : Str -> Try(Str, _)
part1 = |input| {
	monkeys = parse_monkeys(input.trim().split_on("\n\n"), [])?
	final = run_rounds(monkeys, 20)?
	counts = final.map(|monkey| monkey.inspections).sort_with(U64.compare).rev()
	match counts {
		[first, second, ..] => Ok((first * second).to_str())
		_ => Err(NeedAtLeastTwoMonkeys)
	}
}

parse_monkeys : List(Str), List(Monkey) -> Try(List(Monkey), _)
parse_monkeys = |blocks, monkeys|
	match blocks {
		[] => Ok(monkeys)
		[block, .. as rest] => {
			monkey = parse_monkey(block)?
			parse_monkeys(rest, monkeys.append(monkey))
		}
	}

parse_monkey : Str -> Try(Monkey, _)
parse_monkey = |block| {
	lines = block.split_on("\n").map(|line| line.trim())
	match lines {
		[_, items_line, operation_line, test_line, true_line, false_line] => Ok({
			items: parse_items(items_line)?,
			operation: parse_operation(operation_line)?,
			divisor: parse_last_u64(test_line)?,
			if_true: parse_last_u64(true_line)?,
			if_false: parse_last_u64(false_line)?,
			inspections: 0,
		})
		_ => Err(InvalidMonkeyBlock(block))
	}
}

parse_items : Str -> Try(List(U64), _)
parse_items = |line| {
	match line.split_on(": ") {
		["Starting items", values] => parse_item_values(values.split_on(", "), [])
		_ => Err(InvalidItems(line))
	}
}

parse_item_values : List(Str), List(U64) -> Try(List(U64), _)
parse_item_values = |values, items|
	match values {
		[] => Ok(items)
		[value, .. as rest] => {
			item = U64.from_str(value) ? |_| InvalidItem(value)
			parse_item_values(rest, items.append(item))
		}
	}

parse_operation : Str -> Try(Operation, _)
parse_operation = |line|
	match line.split_on(" ") {
		["Operation:", "new", "=", "old", "*", "old"] => Ok(Square)
		["Operation:", "new", "=", "old", "*", value] => Ok(Multiply(U64.from_str(value) ? |_| InvalidOperation(line)))
		["Operation:", "new", "=", "old", "+", value] => Ok(Add(U64.from_str(value) ? |_| InvalidOperation(line)))
		_ => Err(InvalidOperation(line))
	}

parse_last_u64 : Str -> Try(U64, _)
parse_last_u64 = |line| {
	value = line.split_on(" ").last() ? |_| InvalidNumberLine(line)
	number = U64.from_str(value) ? |_| InvalidNumberLine(line)
	Ok(number)
}

run_rounds : List(Monkey), U64 -> Try(List(Monkey), _)
run_rounds = |monkeys, remaining| {
	if remaining == 0 {
		return Ok(monkeys)
	}
	updated = run_round(monkeys, 0)?
	run_rounds(updated, remaining - 1)
}

run_round : List(Monkey), U64 -> Try(List(Monkey), _)
run_round = |monkeys, index| {
	if index >= monkeys.len() {
		return Ok(monkeys)
	}
	updated = take_turn(monkeys, index)?
	run_round(updated, index + 1)
}

take_turn : List(Monkey), U64 -> Try(List(Monkey), _)
take_turn = |monkeys, index| {
	monkey = monkeys.get(index) ? |_| MissingMonkey(index)
	cleared = { ..monkey, items: [], inspections: monkey.inspections + monkey.items.len() }
	without_items = monkeys.set(index, cleared)?
	throw_items(without_items, monkey, monkey.items)
}

throw_items : List(Monkey), Monkey, List(U64) -> Try(List(Monkey), _)
throw_items = |monkeys, source, items|
	match items {
		[] => Ok(monkeys)
		[item, .. as rest] => {
			worry = apply_operation(source.operation, item) / 3
			target_index = if worry % source.divisor == 0 {
				source.if_true
			} else {
				source.if_false
			}
			target = monkeys.get(target_index) ? |_| MissingMonkey(target_index)
			updated_target = { ..target, items: target.items.append(worry) }
			updated = monkeys.set(target_index, updated_target)?
			throw_items(updated, source, rest)
		}
	}

apply_operation : Operation, U64 -> U64
apply_operation = |operation, old|
	match operation {
		Add(value) => old + value
		Multiply(value) => old * value
		Square => old * old
	}

example_input = 
	\\Monkey 0:
	\\  Starting items: 79, 98
	\\  Operation: new = old * 19
	\\  Test: divisible by 23
	\\    If true: throw to monkey 2
	\\    If false: throw to monkey 3
	\\
	\\Monkey 1:
	\\  Starting items: 54, 65, 75, 74
	\\  Operation: new = old + 6
	\\  Test: divisible by 19
	\\    If true: throw to monkey 2
	\\    If false: throw to monkey 0
	\\
	\\Monkey 2:
	\\  Starting items: 79, 60, 97
	\\  Operation: new = old * old
	\\  Test: divisible by 13
	\\    If true: throw to monkey 1
	\\    If false: throw to monkey 3
	\\
	\\Monkey 3:
	\\  Starting items: 74
	\\  Operation: new = old + 3
	\\  Test: divisible by 17
	\\    If true: throw to monkey 0
	\\    If false: throw to monkey 1

## The sample reaches 10,605 monkey-business after twenty rounds.
expect part1(example_input) == Ok("10605")

## Worry levels are adjusted by the parsed operation before relief.
expect apply_operation(Multiply(19), 79) / 3 == 500

## The operation parser recognizes squaring the old value.
expect parse_operation("Operation: new = old * old") == Ok(Square)
