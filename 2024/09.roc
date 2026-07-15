app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.21.0-rc3/Cv3oh6aPxqZxDeh1dgnXPjZnYsoqQMvEawa6TUc71sxJ.tar.zst" }

import pf.OsStr
import pf.Stdin
import pf.Stdout

main! : List(OsStr) => Try({}, _)
main! = |_| {
	input = Str.from_utf8(Stdin.read_to_end!()?) ? |err| InvalidUtf8(err)
	answer = part1(input) ? |err| SolverFailed(err)
	Stdout.line!("Part 1: ${answer}")?
	Ok({})
}

part1 : Str -> Try(Str, [InvalidDigit(U8)])
part1 = |input| {
	dense = parse_dense_format(input)?
	compacted = move_file_blocks(dense, 0, dense.len() - 1)
	score : U64
	score = compacted.fold_with_index(
		0,
		|sum, block, index|
			if block == empty_block {
				sum
			} else {
				sum + index * block.to_u64()
			},
	)
	Ok(score.to_str())
}

## The sample disk checksum is 1928 after block compaction.
expect part1(example_input) == Ok("1928")

example_input = "2333133121414131402"

parse_dense_format : Str -> Try(List(U16), [InvalidDigit(U8)])
parse_dense_format = |input| parse_dense_help(input.trim().to_utf8(), Block(0), [])

parse_dense_help : List(U8), [Free(U16), Block(U16)], List(U16) -> Try(List(U16), [InvalidDigit(U8)])
parse_dense_help = |bytes, next, blocks| {
	match bytes {
		[] => Ok(blocks)
		[first, .. as rest] => {
			if first < '0' or first > '9' {
				Err(InvalidDigit(first))
			} else {
				appended = append_blocks(blocks, next, first - '0')
				parse_dense_help(rest, appended.next, appended.blocks)
			}
		}
	}
}

## Dense-format parsing alternates file and free blocks.
expect {
	actual = parse_dense_format(example_input)?
	actual == [0, 0, empty_block, empty_block, empty_block, 1, 1, 1, empty_block, empty_block, empty_block, 2, empty_block, empty_block, empty_block, 3, 3, 3, empty_block, 4, 4, empty_block, 5, 5, 5, 5, empty_block, 6, 6, 6, 6, empty_block, 7, 7, 7, empty_block, 8, 8, 8, 8, 9, 9]
}

append_blocks : List(U16), [Free(U16), Block(U16)], U8 -> { blocks : List(U16), next : [Free(U16), Block(U16)] }
append_blocks = |blocks, next, len| {
	match next {
		Free(id) => { blocks: blocks.concat(List.repeat(empty_block, len.to_u64())), next: Block(id) }
		Block(id) => { blocks: blocks.concat(List.repeat(id, len.to_u64())), next: Free(id + 1) }
	}
}

## Free spans append empty blocks.
expect append_blocks([], Free(2), 2) == { blocks: [empty_block, empty_block], next: Block(2) }

## File spans append the current file identifier.
expect append_blocks([1, 1], Block(2), 3) == { blocks: [1, 1, 2, 2, 2], next: Free(3) }

move_file_blocks : List(U16), U64, U64 -> List(U16)
move_file_blocks = |blocks, left_free, right_block| {
	match (shift_right(blocks, left_free), shift_left(blocks, right_block)) {
		(Ok(left), Ok(right)) if left < right => {
			match blocks.swap(left, right) {
				Ok(swapped) => move_file_blocks(swapped, left, right)
				Err(_) => blocks
			}
		}
		_ => blocks
	}
}

## Block movement fills free space from the right edge.
expect {
	blocks = [0, empty_block, empty_block, 1, 1, 1, empty_block, empty_block, empty_block, empty_block, 2, 2, 2, 2, 2]
	move_file_blocks(blocks, 0, blocks.len() - 1) == [0, 2, 2, 1, 1, 1, 2, 2, 2, empty_block, empty_block, empty_block, empty_block, empty_block, empty_block]
}

## The sample dense disk compacts to the expected block ordering.
expect {
	blocks = [0, 0, empty_block, empty_block, empty_block, 1, 1, 1, empty_block, empty_block, empty_block, 2, empty_block, empty_block, empty_block, 3, 3, 3, empty_block, 4, 4, empty_block, 5, 5, 5, 5, empty_block, 6, 6, 6, 6, empty_block, 7, 7, 7, empty_block, 8, 8, 8, 8, 9, 9]
	move_file_blocks(blocks, 0, blocks.len() - 1) == [0, 0, 9, 9, 8, 1, 1, 1, 8, 8, 8, 2, 7, 7, 7, 3, 3, 3, 6, 4, 4, 6, 5, 5, 5, 5, 6, 6, empty_block, empty_block, empty_block, empty_block, empty_block, empty_block, empty_block, empty_block, empty_block, empty_block, empty_block, empty_block, empty_block, empty_block]
}

shift_right : List(U16), U64 -> Try(U64, [NoEmptyBlocks])
shift_right = |blocks, index| {
	match blocks.get(index) {
		Ok(block) if block == empty_block => Ok(index)
		Ok(_) if index + 1 < blocks.len() => shift_right(blocks, index + 1)
		_ => Err(NoEmptyBlocks)
	}
}

## Right shifting locates the first empty block.
expect shift_right([1, 1, empty_block, empty_block], 0) == Ok(2)

## Right shifting accepts an already-empty position.
expect shift_right([1, 1, empty_block, empty_block], 2) == Ok(2)

## Right shifting reports a full disk.
expect shift_right([1, 1, 1, 1], 2) == Err(NoEmptyBlocks)

shift_left : List(U16), U64 -> Try(U64, [NoFullBlocks])
shift_left = |blocks, index| {
	match blocks.get(index) {
		Ok(block) if block != empty_block => Ok(index)
		Ok(_) if index > 0 => shift_left(blocks, index - 1)
		_ => Err(NoFullBlocks)
	}
}

## Left shifting locates the final full block.
expect shift_left([1, 1, empty_block, empty_block], 3) == Ok(1)

## Left shifting accepts an already-full position.
expect shift_left([1, 1, empty_block, empty_block], 1) == Ok(1)

## Left shifting reports an empty disk.
expect shift_left([empty_block, empty_block, empty_block, empty_block], 2) == Err(NoFullBlocks)

empty_block : U16
empty_block = 65535
