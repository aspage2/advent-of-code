import gleam/io

import parse

pub fn run() -> Nil {
	let st = parse.split2(", ", "|")

	io.debug(st.run("hello, world|!"))
	Nil
}
