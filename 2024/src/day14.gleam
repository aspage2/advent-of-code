import gleam/bool
import gleam/string_tree
import gleam/option
import gleam/dict
import gleam/list
import gleam/string
import gleam/result
import gleam/int
import gleam/io
import gleam/order.{Eq, Lt, Gt}

import parse.{type Parser}
import util

pub type Vec {
	Vec(x: Int, y: Int)
}

pub type Quadrant {
	I II III IV No
}

pub fn get_quadrant(pos: Vec, bounds: Vec) -> Quadrant {
	let x = int.compare(pos.x, bounds.x/2)
	let y = int.compare(pos.y, bounds.y/2)

	case x, y {
	Eq, _ | _, Eq -> No
	Lt, Lt		  -> I
	Gt, Lt		  -> II
	Lt, Gt		  -> III
	Gt, Gt		  -> IV
	}
}

pub fn parse_line() -> Parser(#(Vec, Vec)) {
	use _ <- parse.do(parse.drop(2))
	use p_x <- parse.do(parse.split_once(","))
	use p_y <- parse.do(parse.split_once(" "))
	use _ <- parse.do(parse.drop(2))
	use v_x <- parse.do(parse.split_once(","))
	use v_y <- parse.do(parse.get())

	let assert Ok(p_x) = int.parse(p_x)
	let assert Ok(p_y) = int.parse(p_y)
	let assert Ok(v_x) = int.parse(v_x)
	let assert Ok(v_y) = int.parse(v_y)

	#(Vec(p_x, p_y), Vec(v_x, v_y))
		|> parse.pure
}

fn translate1(p: Int, v: Int, bound: Int, time: Int) -> Int {
	let new_p = p + time*v
	case new_p < 0 {
	True -> {
		let n = int.absolute_value(new_p)/bound
		{new_p + {n+1}*bound}%bound
	}
	False -> new_p % bound
	}
}

pub fn translate(pos: Vec, vel: Vec, bounds: Vec, time: Int) -> Vec {
	Vec(
		translate1(pos.x, vel.x, bounds.x, time), 
		translate1(pos.y, vel.y, bounds.y, time),
	)
}

pub fn counter(l: List(a)) -> dict.Dict(a, Int) {
	use cnt, x <- list.fold(l, dict.new())
	use opt <- dict.upsert(cnt, x)
	case opt{
		option.None -> 1
		option.Some(x) -> x + 1
	}
}

pub fn print(vs: List(Vec), bounds: Vec) -> String {
	let cnts = counter(vs)
	{
		use sb, y <- list.fold(list.range(0, bounds.y-1), string_tree.new())
		let sb = {
			use sb, x <- list.fold(list.range(0, bounds.x-1), sb)
			let grapheme = case dict.get(cnts, Vec(x, y)) {
			Error(Nil) -> "."
			Ok(x) -> int.to_string(x)
			}
			string_tree.append(sb, grapheme)
		}
		string_tree.append(sb, "\n")
	} |> string_tree.to_string
}

pub fn x_symmetrical(robots: List(Vec), bounds: Vec) -> Bool {
	let center = bounds.x / 2

	let xs = robots
		|> list.map(fn(r){r.x})
		|> counter

	list.range(0, center-1)
		|> list.all(fn(x) {
			let r = {
				use left <- result.try(dict.get(xs, x))
				use right <- result.map(dict.get(xs, bounds.x-1-x))
				left == right
			}
			result.unwrap(r, False)
		})
}

pub fn find_symm(robots: List(#(Vec, Vec)), bounds: Vec, t: Int) -> Int {
	use <- bool.lazy_guard(t > 100000000, fn() {panic})
	let new_robots = robots
		|> list.map(fn(x) {translate(x.0, x.1, bounds, t)})

	case x_symmetrical(new_robots, bounds) {
		True -> t
		False -> find_symm(robots, bounds, t+1)
	}
}

pub fn day_main(day: util.AdventDay) -> Nil {
	let assert Ok(txt) = util.read_file(day.file)
		|> result.map(string.trim)

	let parser = parse_line()

	let robots = txt
		|> string.split("\n")
		|> list.map(parse.eval(parser, _))

	let b = Vec(101, 103)

	let _ = robots
		|> list.map(fn(x) {
			translate(x.0, x.1, b, 100)
		})
		|> list.map(get_quadrant(_, b))
		|> counter
		|> dict.filter(fn(k, _) { k != No })
		|> dict.values
		|> list.reduce(int.multiply)
		|> io.debug

	find_symm(robots, b, 1)
		|> io.debug

	Nil
}
