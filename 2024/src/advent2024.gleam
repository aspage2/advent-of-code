
import day2
import gleam/io
import gleam/string
import gleam/list

pub type Atom

@external(erlang, "init", "get_plain_arguments")
pub fn erl_get_plain_arguments() -> Result(List(List(UtfCodepoint)), Atom)


/// Return command line args
pub fn argv() -> List(String) {
	let assert Ok(args) = erl_get_plain_arguments()

	list.map(args, string.from_utf_codepoints)
}

pub fn main() {
	day2.day_main()
}
