import gleam/io
import gleam/result
import gleam/option
import gleam/int
import gleam/list
import gleam/dict.{type Dict}

import util
import grid.{type Grid, type Cell, Cell}

// Return a dictionary of end-trails mapped to the number of
// trails which lead there.
fn accessible_trails(g: Grid) -> Dict(Cell, Int) {
	// Find all starting cells
	let starts = {
		use r <- list.flat_map(list.range(0, g.height - 1))
		use c <- list.map(list.range(0, g.width - 1))
		Cell(r, c)
	} 
	|> list.filter(fn(c) {
		case grid.at(g, c) {
			Error(_) -> False
			Ok(s) -> s == int.to_string(0)
		}
	})
	|> list.map(fn(c) { #(c, 1) }) // See below.

	// All hiking paths must be length 10, and no trail can loop
	// or backtrack. Because of this, we can create a very simple
	// BFS using a "splitting explorer" strategy
	
	// Fold for 9 rounds. The accumulator is a dictionary of how
	// explorers are on a particular cell. The starting dict contains
	// one explorer for each 0-cell on the grid.
	use visited, i <- list.fold(list.range(1, 9), dict.from_list(starts))
	let i_s = int.to_string(i)
	visited
		|> dict.to_list
		|> list.flat_map(fn(p) {
			// When an explorer explores, they create one copy
			// of themselves for each neighboring node they can
			// visit next. If N explorers are in a cell, they
			// each make a copy-per-neighbor.
			// This results in each neighbor cell containing N
			// explorers.
			grid.cardinal_neighbors(p.0)
				|> list.filter(fn(n) {
					case grid.at(g, n) {
						Ok(s) ->    s == i_s
						Error(_) -> False
					}
				})
				|> list.map(fn(n) {#(n, p.1)})
		})
		|> list.fold(dict.new(), fn(d, p) {
			// After the explorers move, it may be
			// the case that two trails converge.
			// We simply combine the N+M explorers
			// who met up on this cell to create
			// a new group of explorers.
			use val <- dict.upsert(d, p.0)
			case val {
				option.None -> p.1
				option.Some(cnt) -> p.1 + cnt
			}
		})
}

pub fn day_main(day: util.AdventDay) {
	let assert Ok(g) = util.read_file(day.file)
		|> result.replace_error(Nil)
		|> result.then(grid.parse_grid)

	accessible_trails(g)
		|> dict.values
		|> int.sum
		|> io.debug
	Nil
}

