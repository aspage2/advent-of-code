import gleam/string

pub type State(s, a) {
	State(run: fn(s) -> #(a, s))
}

pub fn map(s: State(s, a), f: fn(a) -> b) -> State(s, b) {
	use x <- trans_func()
	let #(tok, new_s) = s.run(x)
	#(f(tok), new_s)
}

pub fn trans_func(tf: fn(s) -> #(a, s)) -> State(s, a) {
	State(tf)
}

pub fn bind(s: State(s, a), f: fn(a) -> State(s, b)) -> State(s, b) {
	use x <- trans_func()

	let #(tok, new_s) = s.run(x)
	let new_st = f(tok)
	new_st.run(new_s)
}

pub fn wrap(x: a) -> State(s, a) {
	use y <- trans_func()
	#(x, y)
}

pub fn set(val: s) -> State(s, Nil) {
	use _ <- trans_func()
	#(Nil, val)
}

pub fn get() -> State(s, s) {
	use s <- trans_func()
	#(s, s)
}

type Parser(a) = State(String, a)

pub fn split_once(on: String) -> Parser(String) {
	use s <- trans_func()
	let assert Ok(pair) = string.split_once(s, on)
	pair
}

pub fn split2(on1: String, on2: String) -> Parser(#(String, String)) {
	use s1 <- bind(split_once(on1))
	use s2 <- map(split_once(on2))
	#(s1, s2)
}
