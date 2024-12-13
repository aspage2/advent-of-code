import gleam/result
import gleam/list
import gleam/bool
import gleam/string
import gleam/int
import gleam/io

import parse.{type Parser}
import util

pub type Mat {
	Mat(a: Int, b: Int, c: Int, d: Int)
}

pub type Vec {
	Vec(x: Int, y: Int)
}

pub fn dot(v1: Vec, v2: Vec) -> Int {
	v1.x*v2.x + v1.y*v2.y
}

pub fn try_cramer(m: Mat, v: Vec) -> Result(Vec, Nil) {
	let m_det = determinant(m)
	let x_det = determinant(Mat(v.x, m.b, v.y, m.d))
	let y_det = determinant(Mat(m.a, v.x, m.c, v.y))

	case x_det % m_det != 0 || y_det % m_det != 0 {
	True -> Error(Nil)
	False -> Vec(x_det / m_det, y_det/m_det) |> Ok
	}
}
pub fn take_button_line() -> Parser(#(Int, Int)) {
	use _ <- parse.do(parse.drop(12))
	use #(x_s, y_s) <- parse.map(parse.split2(", Y+", "\n"))

	let assert Ok(x) = int.parse(x_s)
	let assert Ok(y) = int.parse(y_s)
	#(x, y)
}

pub fn take_prize_line() -> Parser(#(Int, Int)) {
	use _ <- parse.do(parse.drop(9))
	use #(x_s, y_s) <- parse.map(parse.split2(", Y=", "\n"))

	let assert Ok(x) = int.parse(x_s)
	let assert Ok(y) = int.parse(y_s)
	#(x+10000000000000, y+10000000000000)
}

pub fn take_system() -> Parser(#(Mat, Vec)) {
	use #(ax, ay) <- parse.do(take_button_line())
	use #(bx, by) <- parse.do(take_button_line())
	use #(cx, cy) <- parse.map(take_prize_line())

	#(Mat(ax, bx, ay, by), Vec(cx, cy))
}

pub fn take_all() -> Parser(List(#(Mat, Vec))) {
	use curr <- parse.do(parse.get())
	use <- bool.guard(string.trim(curr) == "", parse.pure([])) 

	use pair <- parse.do(take_system())
	use rest <- parse.map(take_all())
	[pair, ..rest]
}

pub fn determinant(m: Mat) -> Int {
	m.a * m.d - m.b * m.c
}

pub fn day_main(day: util.AdventDay) {
	let assert Ok(txt) = util.read_file(day.file)

	let #(systems, _) = take_all().run(txt)
	systems
		|> list.map(fn(p) {try_cramer(p.0, p.1)})
		|> result.values
		|> list.map(dot(Vec(3, 1), _))
		|> int.sum
		|> io.debug
	Nil
}
