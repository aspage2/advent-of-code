
import gleam/int
import gleam/io
import gleam/result
import gleam/string
import gleam/dict
import gleam/bool
import gleam/list

import grid.{type Grid, type Cell, Cell}
import counter.{type Counter}
import util

pub fn get_info(g: Grid) -> #(Cell, Cell) {
  let base = #(Cell(0, 0), Cell(0, 0))
  use p, r <- list.fold(list.range(0, g.height-1), base)
  use #(st, end) as p, c <- list.fold(list.range(0, g.width-1), p)
  let c = Cell(r, c)
  case grid.at(g, c) {
  Error(_) -> panic
  Ok("S") -> #(c, end)
  Ok("E") -> #(st, c)
  _ -> p
  }
}

// The Grid is guaranteed to have exactly  one path from
// start to end without any dead-ends (verified through
// traversing the input). This in mind, we can make a few
// data structures that will make our lives easier down the
// road:
// First, a "trace" dictionary that maps the distance from
// any given open space in the grid to the end space (i.e.
// the amount of time it would take to traverse without cheating.
// Second, a list of cells in the order of path traversal, for
// folding over when finding all cheats.
// Both structures can be easily built by traversing the maze
// backwards.
// 
// To "cheat" is essientially to move your character <r,c>
// units, where |r| + |c| <= your max cheat duration, to an
// open space. So every unique "cheat" is every space you
// can get to from space A within the taxicab limit. The time
// of the cheat path is the time it took to get to space A, plus
// the cheat time, plus the time it would take to navigate from
// the cheat landing space (which is easy to look up in the trace
// dict).
//
// By setting up a list and trace dictionary, the problem boils
// down to an index_fold, where the index is the current travel
// time and the accumulator is a counter of cheat times.

fn trace_from_rec(
  g: Grid, 
  iter: Int, 
  prev: Cell,
  curr: Cell,
  acc: #(List(Cell), dict.Dict(Cell, Int)),
) -> #(List(Cell), dict.Dict(Cell, Int)) {
  let #(l, d) = acc

  let x = grid.cardinal_neighbors(curr) 
    |> list.filter(fn(c) {
      case grid.at(g, c) {
      Error(_)|Ok("#") -> False
      _ -> c != prev
      }
    })
  let acc = #([curr, ..l], dict.insert(d, curr, iter))

  case x {
    [] -> acc
    [next, ..] -> trace_from_rec(g, iter+1, curr, next, acc)
  }
}

fn trace_from(g: Grid, start: Cell) 
  -> #(List(Cell), dict.Dict(Cell, Int)) {
  trace_from_rec(g, 0, Cell(-1, -1), start, #([], dict.new()))
}

fn count_cheats(
  path: List(Cell),
  cheat: Int,
  trace: dict.Dict(Cell, Int), 
) -> Counter(Int) {
  use acc, curr, iter <- list.index_fold(path, counter.new())
  {
    use r <- list.flat_map(list.range(-cheat, cheat))
    use c <- list.map(list.range(
      int.absolute_value(r)-cheat, cheat - int.absolute_value(r)
    ))
    #(r, c)
  } 
    |> list.flat_map(fn(p) {
      let #(r, c) = p
      use <- bool.guard(r == 0 && c == 0, [])
      let nc = Cell(curr.r+r, curr.c+c)
      case dict.get(trace, nc) {
        Error(_) -> []
        Ok(dst) -> [int.absolute_value(r) + int.absolute_value(c) + iter + dst]
      }
    })
    |> counter.extend(acc, _)
}

pub fn day_main(day: util.AdventDay) {
  let assert Ok(g) = util.read_file(day.file)
    |> result.map(string.trim)
    |> result.replace_error(Nil)
    |> result.then(grid.parse_grid)

  let #(st, end) = get_info(g)


  let #(path, trace) = trace_from(g, end)

  let assert Ok(start_num) = dict.get(trace, st)

  io.debug(start_num)

  count_cheats(path, 20, trace)
    |> dict.filter(fn(k, _){ 
      start_num - k >= 100
    })
    |> dict.values
    |> int.sum
    |> io.debug
  Nil
}
