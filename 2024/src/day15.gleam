import gleam/int
import gleam/io
import gleam/string_tree
import gleam/string
import gleam/result
import gleam/bool
import gleam/list
import gleam/set

import util
import grid.{type Grid, type Cell, Cell, type Dir, N, S, E, W}

type Info {
	Info(boxes: set.Set(Cell), walls: set.Set(Cell))
}

fn get_info(g: Grid) -> #(Info, Cell) {
	let cells = {
		use r <- list.flat_map(list.range(0, g.height - 1))
		use c <- list.map(list.range(0, g.width - 1))
		Cell(r, c)
	}
	use #(inf, st), i <- list.fold(cells, #(Info(set.new(), set.new()), Cell(-1, -1)))
	case grid.at(g, i) {
	Error(_) -> panic 
	Ok("#") -> #(Info(..inf, walls: set.insert(inf.walls, i)), st)
	Ok("O") -> #(Info(..inf, boxes: set.insert(inf.boxes, i)), st)
	Ok("@") -> #(inf, i)
	_ -> #(inf, st)
	}
}

fn get_info2(g: Grid) -> #(Info, Cell) {
	let cells = {
		use r <- list.flat_map(list.range(0, g.height - 1))
		use c <- list.map(list.range(0, g.width - 1))
		Cell(r, c)
	}
	use #(inf, st), i <- list.fold(cells, #(Info(set.new(), set.new()), Cell(-1, -1)))
	let i1 = Cell(i.r, i.c*2)
	let i2 = Cell(i.r, i.c*2+1)
	case grid.at(g, i) {
	Error(_) -> panic 
	Ok("#") -> #(Info(..inf, walls: set.insert(inf.walls, i1) |> set.insert(i2)), st)
	Ok("O") -> #(Info(..inf, boxes: set.insert(inf.boxes, i1)), st)
	Ok("@") -> #(inf, i1)
	_ -> #(inf, st)
	}
}

fn is_wall(inf: Info, c: Cell) -> Bool {
	set.contains(inf.walls, c)
}

fn is_box(inf: Info, c: Cell) -> Bool {
	set.contains(inf.boxes, c)
}

fn move_box(inf: Info, loc: Cell, d: Dir) -> Result(Info, Nil) {
	let mb = fn(i, old: Cell, new: Cell) -> Info {
		Info(..i, boxes: i.boxes
			|> set.insert(new)
			|> set.delete(old)
		)
	}
	let new_loc = grid.move(loc, d)
	use <- bool.guard(is_wall(inf, new_loc), Error(Nil))
	use <- bool.guard(!is_box(inf, new_loc), Ok(mb(inf, loc, new_loc)))

	move_box(inf, new_loc, d)
		|> result.map(fn(new_inf) {
			mb(new_inf, loc, new_loc)
		})
}

fn move_guy(inf: Info, loc: Cell, d: Dir) -> Result(#(Info, Cell), Nil) {
	let new_loc = grid.move(loc, d)
	use <- bool.guard(is_wall(inf, new_loc), Error(Nil))
	use <- bool.guard(!is_box(inf, new_loc), Ok(#(inf, new_loc)))

	move_box(inf, new_loc, d)
		|> result.map(fn(new_inf) { #(new_inf, new_loc) })

}

fn get_box_2(inf: Info, c: Cell) -> Result(Cell, Nil) {
	use <- bool.guard(set.contains(inf.boxes, c), Ok(c))
	let c = grid.move(c, W)
	use <- bool.guard(set.contains(inf.boxes, c), Ok(c))
	Error(Nil)
}

fn get_boxes_rec(inf: Info, st: Cell, dir: Dir, to_visit: List(Cell), vs: set.Set(Cell)) -> List(Cell) {
	use <- bool.lazy_guard(to_visit == [], fn() {
		vs |> set.to_list
	})
	let new_to_visit = case dir {
	N | S -> to_visit
		|> list.flat_map(fn(c) {
			let new_c = grid.move(c, dir)
			let a = get_box_2(inf, new_c)
				|> result.map(list.wrap)
				|> result.unwrap([])
			let b = get_box_2(inf, grid.move(new_c, E))
				|> result.map(list.wrap)
				|> result.unwrap([])
			list.append(a, b)
		})
		|> set.from_list
		|> set.difference(vs)
	E -> {
		to_visit
			|> list.flat_map(fn(c) {
				let new_c = c |> grid.move(dir) |> grid.move(dir)
				get_box_2(inf, new_c) |> result.map(list.wrap) |> result.unwrap([])
			})
			|> set.from_list
		|> set.difference(vs)
	}
	W -> {
		to_visit
			|> list.flat_map(fn(c) {
				let new_c = c |> grid.move(dir)
				get_box_2(inf, new_c) |> result.map(list.wrap) |> result.unwrap([])
			})
			|> set.from_list
			|> set.difference(vs)
	} }

	get_boxes_rec(inf, st, dir, new_to_visit |> set.to_list, vs |> set.union(new_to_visit))
}

fn get_boxes(inf: Info, st: Cell, dir: Dir) -> List(Cell) {
	case get_box_2(inf, st) {
		Error(_) -> []
		Ok(c)    ->	get_boxes_rec(inf, st, dir, [c], set.from_list([c]))
	}
}


fn move_guy_2(inf: Info, loc: Cell, d: Dir) -> Result(#(Info, Cell), Nil) {
	let new_loc = grid.move(loc, d)
	// If it's  a wall, don't move.
	use <- bool.guard(is_wall(inf, new_loc), Ok(#(inf, loc)))

	case get_boxes(inf, new_loc, d) {
	[] -> Ok(#(inf, new_loc))
	boxes -> {
		let t = boxes
			|> list.any(fn(c) {
				let new_c = grid.move(c, d)
				case d {
				N | S -> is_wall(inf, new_c) || is_wall(inf, grid.move(new_c, E))
				E -> is_wall(inf, new_c |> grid.move(E))
				W -> is_wall(inf, grid.move(c, W))
			}})

		case t {
		True -> Ok(#(inf, loc))
		False -> {
			let boxes = set.from_list(boxes)
			let new_boxes = boxes
				|> set.map(fn(b) {grid.move(b, d)})

			Ok(#(
				Info(
					..inf,
					boxes: inf.boxes 
						|> set.difference(boxes) 
						|> set.union(new_boxes)
				), 
				new_loc,
			))
		}
	}}}
}

fn parse(txt: String) -> #(Info, Cell, List(Dir), Grid) {
	let assert Ok(#(g, inst)) = string.split_once(txt, "\n\n")
	let assert Ok(g) = grid.parse_grid(g|>string.trim)

	let #(i, st) = get_info(g)

	let ds = inst
		|> string.trim
		|> string.split("\n")
		|> string_tree.from_strings
		|> string_tree.to_string
		|> string.to_graphemes
		|> list.map(fn(c) { case c {
			"^" -> N
			"v" -> S
			"<" -> W
			">" -> E
			_ -> panic
		}})
		#(i, st, ds, g)
}

fn parse2(txt: String) -> #(Info, Cell, List(Dir), Grid) {
	let assert Ok(#(g, inst)) = string.split_once(txt, "\n\n")
	let assert Ok(g) = grid.parse_grid(g|>string.trim)

	let #(i, st) = get_info2(g)

	let ds = inst
		|> string.trim
		|> string.split("\n")
		|> string_tree.from_strings
		|> string_tree.to_string
		|> string.to_graphemes
		|> list.map(fn(c) { case c {
			"^" -> N
			"v" -> S
			"<" -> W
			">" -> E
			_ -> panic
		}})
		#(i, st, ds, g)
}

fn to_string(g: Grid, i: Info, pos: Cell) {
	{
		use sb, r <- list.fold(list.range(0, g.height - 1), string_tree.new()) list.fold(list.range(0, g.width - 1), sb, fn(sb, c) {
			let c = Cell(r, c)
			string_tree.append(sb, {
				use <- bool.guard(is_wall(i, c), "#")
				use <- bool.guard(is_box(i, c), "O")
				use <- bool.guard(c == pos, "X")
				"."
			})
		}) |> string_tree.append("\n")
	}|> string_tree.to_string
}

fn to_string_2(g: Grid, i: Info, pos: Cell) {
	{
		use sb, r <- list.fold(list.range(0, g.height - 1), string_tree.new())
		list.fold(list.range(0, g.width * 2 - 1), sb, fn(sb, c) {
			let c = Cell(r, c)
			{
				use <- bool.guard(is_wall(i, c), "#")
				use <- bool.guard(c == pos, "@")
				case get_box_2(i, c) {
				Error(_) -> "."
				Ok(c2) -> case c2 != c {
					True -> "]"
					False -> "["
				}}
			} |> string_tree.append(sb, _)
		}) |> string_tree.append("\n")
	} |> string_tree.to_string
}

fn gps(inf: Info) -> Int {
	inf.boxes
		|> set.to_list
		|> list.map(fn(cell) {
			100*cell.r + cell.c
		})
		|> int.sum()
}

@external(erlang, "io", "get_line")
fn erl_get_line(prompt: String) -> Result(String, Nil)

fn wait() -> Nil {
	erl_get_line("")
	Nil
}

pub fn day_main(day: util.AdventDay) -> Nil {
	let assert Ok(txt) = util.read_file(day.file)
	let #(i, s, ds, g) = parse2(txt)
	io.debug(s)
	to_string_2(g, i, s) |> io.println

	let f = fn(i, s, d) {
		move_guy_2(i, s, d)
			|> result.unwrap(#(i, s))
	}

	let #(i, s) = ds
		|> list.fold(#(i, s), fn(p, d) {
			let n = f(p.0, p.1, d)
			//io.println("\u{1b}[2J\u{1b}[H")
			//io.println(string.inspect(d))
			//io.println(to_string_2(g, n.0, n.1))
			//wait()
			n
		})
	
	to_string_2(g, i, s)
		|> io.println

	io.debug(gps(i))

	Nil
}
