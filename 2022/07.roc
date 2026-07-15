app [main!] {
	pf: platform "../../basic-cli/platform/main.roc",
}

import pf.OsStr
import pf.Stdin
import pf.Stdout

FileSystem : { cwd : List(Str), sizes : Dict(Str, U64) }

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
	filesystem = parse_terminal_output(input)?
	total = filesystem.sizes.values().keep_if(|size| size <= 100_000).sum()
	Ok(total.to_str())
}

parse_terminal_output : Str -> Try(FileSystem, _)
parse_terminal_output = |input| parse_lines(input.trim().split_on("\n"), { cwd: [], sizes: Dict.empty() })

parse_lines : List(Str), FileSystem -> Try(FileSystem, _)
parse_lines = |lines, state|
	match lines {
		[] => Ok(state)
		[line, .. as rest] => {
			updated = process_line(state, line)?
			parse_lines(rest, updated)
		}
	}

process_line : FileSystem, Str -> Try(FileSystem, _)
process_line = |state, line| {
	parts = line.split_on(" ")
	match parts {
		["$", "cd", "/"] => Ok({ ..state, cwd: [] })
		["$", "cd", ".."] => Ok({ ..state, cwd: state.cwd.drop_last(1) })
		["$", "cd", directory] => Ok({ ..state, cwd: state.cwd.append(directory) })
		["$", "ls"] => Ok(state)
		["dir", _] => Ok(state)
		[size_str, _] => {
			size = U64.from_str(size_str) ? |_| InvalidListing(line)
			Ok({ ..state, sizes: add_to_ancestors(state.sizes, state.cwd, size, 0) })
		}
		_ => Err(InvalidListing(line))
	}
}

add_to_ancestors : Dict(Str, U64), List(Str), U64, U64 -> Dict(Str, U64)
add_to_ancestors = |sizes, cwd, size, depth| {
	if depth > cwd.len() {
		return sizes
	}
	path = if depth == 0 {
		"/"
	} else {
		"/${Str.join_with(cwd.take_first(depth), "/")}"
	}
	current = sizes.get(path).ok_or(0)
	add_to_ancestors(sizes.insert(path, current + size), cwd, size, depth + 1)
}

example_input = 
	\\$ cd /
	\\$ ls
	\\dir a
	\\14848514 b.txt
	\\8504156 c.dat
	\\dir d
	\\$ cd a
	\\$ ls
	\\dir e
	\\29116 f
	\\2557 g
	\\62596 h.lst
	\\$ cd e
	\\$ ls
	\\584 i
	\\$ cd ..
	\\$ cd ..
	\\$ cd d
	\\$ ls
	\\4060174 j
	\\8033020 d.log
	\\5626152 d.ext
	\\7214296 k

## Small directories contribute their complete recursive sizes.
expect part1(example_input) == Ok("95437")

## Parsing tracks the recursive size of the root directory.
expect {
	filesystem = parse_terminal_output(example_input)?
	filesystem.sizes.get("/")? == 48381165
}
