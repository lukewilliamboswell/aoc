app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.21.0-rc3/Cv3oh6aPxqZxDeh1dgnXPjZnYsoqQMvEawa6TUc71sxJ.tar.zst" }

import pf.OsStr
import pf.Stdin
import pf.Stdout

BitCounts : { zeros : U64, ones : U64 }

main! : List(OsStr) => Try({}, _)
main! = |_args| {
	bytes = Stdin.read_to_end!()?
	input = Str.from_utf8(bytes) ? |err| InvalidUtf8(err)
	answer1 = part1(input) ? |err| SolverFailed(Str.inspect(err))
	answer2 = part2(input) ? |err| SolverFailed(Str.inspect(err))
	Stdout.line!("Part 1: ${answer1}")?
	Stdout.line!("Part 2: ${answer2}")?
	Ok({})
}

part1 : Str -> Try(Str, [NoDiagnosticRows])
part1 = |input| {
	numbers = parse_input(input)
	counts = count_bits(numbers)?
	gamma = compare_bit_counts(counts, Gamma)
	epsilon = compare_bit_counts(counts, Epsilon)
	power = binary_to_decimal(gamma) * binary_to_decimal(epsilon)
	Ok("The gamma:${gamma}, epsilon:${epsilon}, power:${power.to_str()}")
}

# The legacy solution did not yet implement oxygen/CO2 filtering, so preserve its output.
part2 : Str -> Try(Str, [NoDiagnosticRows])
part2 = |input| part1(input)

binary_to_decimal : Str -> U64
binary_to_decimal = |binary|
	binary.to_utf8().fold(0, |total, byte| total * 2 + if byte == '1' 1 else 0)

parse_input : Str -> List(List(U8))
parse_input = |contents|
	contents
		.trim()
		.split_on("\n")
		.map(|line| line.to_utf8())
		.keep_if(|row| !row.is_empty())

count_bits : List(List(U8)) -> Try(List(BitCounts), [NoDiagnosticRows])
count_bits = |rows| {
	first = rows.first() ? |_| NoDiagnosticRows
	initial = List.repeat({ zeros: 0, ones: 0 }, first.len())
	Ok(
		rows.fold(
			initial,
			|counts, row|
				row.fold_with_index(
					counts,
					|next, byte, index| {
						current = next.get(index) ?? { zeros: 0, ones: 0 }
						updated = if byte == '1' {
							{ ..current, ones: current.ones + 1 }
						} else {
							{ ..current, zeros: current.zeros + 1 }
						}
						next.set(index, updated) ?? next
					},
				),
		),
	)
}

compare_bit_counts : List(BitCounts), [Gamma, Epsilon] -> Str
compare_bit_counts = |counts, policy| {
	digits = counts.map(
		|{ zeros, ones }|
			match policy {
				Gamma => if ones > zeros "1" else "0"
				Epsilon => if ones > zeros "0" else "1"
			},
	)
	"0b${Str.join_with(digits, "")}"
}

example = 
	\\00100
	\\11110
	\\10110
	\\10111
	\\10101
	\\01111
	\\00111
	\\11100
	\\10000
	\\11001
	\\00010
	\\01010

## Binary strings convert to decimal values.
expect binary_to_decimal("0b010111011111") == 1503

## The sample diagnostic report produces the expected power consumption.
expect part1(example)? == "The gamma:0b10110, epsilon:0b01001, power:198"

## Both policies choose opposite bits at each position.
expect {
	counts = [{ zeros: 2, ones: 1 }, { zeros: 1, ones: 2 }, { zeros: 2, ones: 1 }]
	compare_bit_counts(counts, Gamma) == "0b010" and compare_bit_counts(counts, Epsilon) == "0b101"
}
