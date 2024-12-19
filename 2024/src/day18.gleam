import gleam/io
import gleam/bool
import gleam/set
import gleam/result
import gleam/list
import gleam/int
import gleam/string
import util

import grid.{type Grid, Grid, Cell, type Cell}

fn parse_txt(txt: String) -> Result(List(Cell), Nil) {
	let lines = txt
		|> string.trim
		|> string.split("\n")
	
	let res = {
		use l <- list.map(lines)
		use #(r, c) <- result.try(string.split_once(l, ","))
		use c <- result.try(int.parse(c))
		use r <- result.map(int.parse(r))
		Cell(r, c)
	} |> result.all()

	res
}

pub fn bfs_rec(
	g: Grid,
	bad_cells: set.Set(Cell), 
	iter: Int,
	to_visit: set.Set(Cell), 
	visited: set.Set(Cell),
) -> Result(Int, Nil) {
	use <- bool.guard(set.is_empty(to_visit), Error(Nil))
	let end_cell = Cell(g.height - 1, g.width - 1)
	use <- bool.guard(set.contains(to_visit, end_cell), Ok(iter))
	let next_nodes = to_visit
		|> set.to_list
		|> list.flat_map(grid.cardinal_neighbors)
		|> list.filter(fn(n) {
				grid.contains(g, n)
		})
		|> set.from_list
		|> set.difference(visited)
		|> set.difference(bad_cells)

	bfs_rec(g, bad_cells, iter+1, next_nodes, visited |> set.union(next_nodes))
}

pub fn bfs(g: Grid, bad_cells: set.Set(Cell)) -> Result(Int, Nil) {
	let is = set.from_list([Cell(0, 0)])
	bfs_rec(g, bad_cells, 0, is, is)
}

fn chk(g: Grid, b: set.Set(Cell), r: List(Cell), m: Int) -> Bool {
	let new_bad = r
		|> list.take(m)
		|> set.from_list
		|> set.union(b)
		
	result.is_ok(bfs(g, new_bad))
	
}

pub fn find_it(
	g: Grid, 
	bad: set.Set(Cell), 
	rest: List(Cell),
	min: Int,
	max: Int,
) -> Result(Cell, Nil) {
	io.debug(#(min, max))
	use <- bool.lazy_guard(min >= max, fn(){
		let v = case chk(g, bad, rest, min) {
		True -> min + 1
		False -> min
		}
		let assert [h, ..] = list.drop(rest, v-1)
		io.debug(h)
		io.debug(chk(g, bad, rest, v-1))
		let assert [h, ..] = list.drop(rest, v)
		io.debug(h)
		io.debug(chk(g, bad, rest, v))
		let assert [h, ..] = list.drop(rest, v+1)
		io.debug(h)
		io.debug(chk(g, bad, rest, v+1))
		let assert [h, ..] = list.drop(rest, v+2)
		io.debug(h)
		io.debug(chk(g, bad, rest, v+2))
		Ok(h)
	})
	let mid = {min + max} / 2
	case chk(g, bad, rest, mid) {
	True -> find_it(g, bad, rest, mid+1, max)
	False -> find_it(g, bad, rest, min, mid-1)
	}
}

pub fn day_main(day: util.AdventDay) -> Nil {
	let assert Ok(cells) = util.read_file(day.file)
		|> result.replace_error(Nil)
		|> result.then(parse_txt)
	
	let #(st, rest) = cells |> list.split(1024)
	let g = Grid(<<>>, 71, 71)
	find_it(g, set.from_list(st), rest, 0, list.length(rest) - 1)
		|> io.debug

	Nil
}
