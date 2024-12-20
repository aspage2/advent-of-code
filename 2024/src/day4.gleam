import gleam/bit_array
import gleam/bool
import gleam/bytes_tree
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string

import util

pub type Grid {
  Grid(data: BitArray, width: Int, height: Int)
}

pub fn new_grid(data: String) -> Grid {
  let lines = string.split(data, "\n")
  let width = list.length(lines)
  let assert [fst, ..] = lines
  let height = string.length(fst)

  let ba =
    lines
    |> list.map(bit_array.from_string)
    |> bytes_tree.concat_bit_arrays
    |> bytes_tree.to_bit_array

  Grid(ba, width, height)
}

pub fn grid_at(g: Grid, r: Int, c: Int) -> Int {
  let ind = g.width * r + c
  let assert <<_:unit(8)-size(ind), a, _:bytes>> = g.data
  a
}

pub fn p1_matches(g: Grid, r: Int, c: Int) -> Int {
  use <- bool.guard(!{ r < g.height && c < g.width }, 0)

  let horiz =
    c <= g.width - 4
    && {
      let x = <<
        grid_at(g, r, c),
        grid_at(g, r, c + 1),
        grid_at(g, r, c + 2),
        grid_at(g, r, c + 3),
      >>
      x == <<"XMAS">> || x == <<"SAMX">>
    }
  let vert =
    r <= g.height - 4
    && {
      let x = <<
        grid_at(g, r, c),
        grid_at(g, r + 1, c),
        grid_at(g, r + 2, c),
        grid_at(g, r + 3, c),
      >>
      x == <<"XMAS">> || x == <<"SAMX">>
    }
  let diag =
    c <= g.width - 4
    && r <= g.height - 4
    && {
      let x = <<
        grid_at(g, r, c),
        grid_at(g, r + 1, c + 1),
        grid_at(g, r + 2, c + 2),
        grid_at(g, r + 3, c + 3),
      >>
      x == <<"XMAS">> || x == <<"SAMX">>
    }
  let updiag =
    c <= g.width - 4
    && r >= 3
    && {
      let x = <<
        grid_at(g, r, c),
        grid_at(g, r - 1, c + 1),
        grid_at(g, r - 2, c + 2),
        grid_at(g, r - 3, c + 3),
      >>
      x == <<"XMAS">> || x == <<"SAMX">>
    }
  list.count([horiz, vert, diag, updiag], fn(x) { x })
}

fn points(w: Int, h: Int) -> List(#(Int, Int)) {
  use r <- list.flat_map(list.range(0, h - 1))
  use c <- list.map(list.range(0, w - 1))
  #(r, c)
}

fn p2_matches(g: Grid, r: Int, c: Int) -> Bool {
  use <- bool.guard(r > g.height - 3 || c > g.width - 3, False)

  let x = <<
    grid_at(g, r, c),
    grid_at(g, r + 1, c + 1),
    grid_at(g, r + 2, c + 2),
  >>
  let y = <<
    grid_at(g, r + 2, c),
    grid_at(g, r + 1, c + 1),
    grid_at(g, r, c + 2),
  >>

  { x == <<"SAM">> || x == <<"MAS">> } && { y == <<"SAM">> || y == <<"MAS">> }
}

pub fn day_main(day: util.AdventDay) {
  let assert Ok(grid) =
    util.read_file(day.file)
    |> result.map(string.trim)
    |> result.map(new_grid)

  points(grid.width, grid.height)
  |> list.map(fn(pair) { p1_matches(grid, pair.0, pair.1) })
  |> int.sum
  |> io.debug

  points(grid.width, grid.height)
  |> list.count(fn(pair) { p2_matches(grid, pair.0, pair.1) })
  |> io.debug

  Nil
}

pub fn fuck(ints: List(Int)) {
  case ints {
    [] -> Nil
    _ -> {
      let #(a, rest) = list.split(ints, 10)
      io.debug(a)
      fuck(rest)
    }
  }
}
