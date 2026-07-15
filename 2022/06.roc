app [main!] {
	pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.21.0-rc4/FvCh4vdqm3nBY6DWEfZ8RuGCVfjuMY43HA8KSNk9qVDn.tar.zst",
}

import pf.OsStr
import pf.Stdin
import pf.Stdout

main! : List(OsStr) => Try({}, _)
main! = |_| {
	bytes = Stdin.read_to_end!()?
	input = Str.from_utf8(bytes) ? |err| InvalidUtf8(err)
	answer1 = part1(input) ? |err| SolverFailed(Str.inspect(err))
	answer2 = part2(input) ? |err| SolverFailed(Str.inspect(err))
	Stdout.line!("Part 1: ${answer1}")?
	Stdout.line!("Part 2: ${answer2}")?
	Ok({})
}

part1 : Str -> Try(Str, _)
part1 = |input| {
	index = find_marker(input.trim().to_utf8(), 4)?
	Ok(index.to_str())
}

part2 : Str -> Try(Str, _)
part2 = |input| {
	index = find_marker(input.trim().to_utf8(), 14)?
	Ok(index.to_str())
}

find_marker : List(U8), U64 -> Try(U64, [MarkerNotFound])
find_marker = |buffer, width| find_marker_at(buffer, width, 0)

find_marker_at : List(U8), U64, U64 -> Try(U64, [MarkerNotFound])
find_marker_at = |buffer, width, index| {
	if index + width > buffer.len() {
		return Err(MarkerNotFound)
	}
	window = buffer.sublist({ start: index, len: width })
	if Set.from_list(window).len() == width {
		Ok(index + width)
	} else {
		find_marker_at(buffer, width, index + 1)
	}
}

## A four-byte packet marker is found after the seventh byte.
expect find_marker("mjqjpqmgbljsphdztnvjfqwrcgsmlb".to_utf8(), 4) == Ok(7)

## A packet marker can occur near the beginning of a stream.
expect find_marker("bvwbjplbgvbhsrlpgdmjqwftvncz".to_utf8(), 4) == Ok(5)

## A fourteen-byte message marker is found after the nineteenth byte.
expect find_marker("mjqjpqmgbljsphdztnvjfqwrcgsmlb".to_utf8(), 14) == Ok(19)

## A stream without enough unique bytes has no marker.
expect find_marker("aaaaaaaaaaaaaaaa".to_utf8(), 14) == Err(MarkerNotFound)
