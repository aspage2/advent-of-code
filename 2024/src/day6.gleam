import gleam/bit_array
import gleam/bool
import gleam/bytes_tree
import gleam/io
import gleam/yielder
import gleam/list
import gleam/set
import gleam/string

import util

fn find_guard_rec(n: Int, ba: BitArray) -> Int {
  let assert <<head, rest:bytes>> = ba
  case head {
    94 -> n
    _ -> find_guard_rec(n + 1, rest)
  }
}

fn make_grid(txt: String) -> #(Grid, Cell) {
  let lines = string.split(txt, "\n")
  let assert [line1, ..] = lines
  let width = string.length(line1)
  let height = list.length(lines)

  let ba =
    lines
    |> list.map(bit_array.from_string)
    |> bytes_tree.concat_bit_arrays
    |> bytes_tree.to_bit_array

  let guard_ind = find_guard_rec(0, ba)

  #(Grid(ba, width, height), #(guard_ind / width, guard_ind % width))
}

type Grid {
  Grid(data: BitArray, width: Int, height: Int)
}

fn contains(g: Grid, cel: Cell) -> Bool {
  let #(r, c) = cel

  r >= 0 && r < g.height && c >= 0 && c < g.width
}

fn at(g: Grid, cel: Cell) -> Result(String, Nil) {
  let #(r, c) = cel
  case !contains(g, cel) {
    True -> Error(Nil)
    False -> {
      let ind = g.width * r + c
      let assert <<_:bytes-size(ind), x, _:bytes>> = g.data
      <<x>> |> bit_array.to_string
    }
  }
}

/// Create a new grid with the given cell blocked.
/// be careful with this function, as it creates an
/// entire copy of the grid on each call
fn set(g: Grid, at: Cell) -> Grid {
  let #(r, c) = at
  let ind = g.width * r + c
  let assert <<head:bytes-size(ind), _, rest:bytes>> = g.data

  bytes_tree.concat_bit_arrays([head, <<"#">>, rest])
  |> bytes_tree.to_bit_array
  |> Grid(g.width, g.height)
}

type Cell =
  #(Int, Int)

type Direction {
  North
  South
  East
  West
}

fn right_turn(d: Direction) -> Direction {
  case d {
    North -> East
    East -> South
    South -> West
    West -> North
  }
}

type State {
  State(loc: Cell, dir: Direction)
}

fn ahead(s: State) -> Cell {
  case s.dir {
    North -> #(s.loc.0 - 1, s.loc.1)
    South -> #(s.loc.0 + 1, s.loc.1)
    West -> #(s.loc.0, s.loc.1 - 1)
    East -> #(s.loc.0, s.loc.1 + 1)
  }
}

fn next(s: State, g: Grid) -> Result(State, Nil) {
  let ahd = ahead(s)
  case at(g, ahd) {
    Error(_) -> Error(Nil)
    Ok("#") -> Ok(State(..s, dir: right_turn(s.dir)))
    Ok(_) -> Ok(State(..s, loc: ahd))
  }
}

fn simulate(acc: set.Set(Cell), s: State, g: Grid) -> set.Set(Cell) {
  let new_acc = set.insert(acc, s.loc)
  let n = next(s, g)
  case n {
    Error(_) -> new_acc
    Ok(new_s) -> simulate(new_acc, new_s, g)
  }
}

fn has_loop(walklist: set.Set(State), s: State, g: Grid) -> Bool {
  use <- bool.guard(set.contains(walklist, s), True)

  let walklist = set.insert(walklist, s)
  case next(s, g) {
    Error(_) -> False
    Ok(new_s) -> has_loop(walklist, new_s, g)
  }
}

fn count_loops(guard_start: State, g: Grid) -> Int {
  let rows = yielder.range(0, g.height - 1)
  let cols = yielder.range(0, g.width - 1)
  let pairs = {
    use r <- yielder.flat_map(rows)
    use c <- yielder.map(cols)
    #(r, c)
  }

  use tot, cel <- yielder.fold(pairs, 0)
  let new_g = set(g, cel)
  tot + { has_loop(set.new(), guard_start, new_g) |> bool.to_int }
}

pub fn day_main(day: util.AdventDay) -> Nil {
  let assert Ok(txt) = util.read_file(day.file)
  let #(g, st) = string.trim(txt) |> make_grid
  let guard_start = State(loc: st, dir: North)
  io.debug(set.size(simulate(set.new(), guard_start, g)))
  io.debug(count_loops(guard_start, g))
  Nil
}
