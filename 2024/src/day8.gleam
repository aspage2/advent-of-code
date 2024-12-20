import gleam/bool
import gleam/dict
import gleam/io
import gleam/iterator
import gleam/list
import gleam/option
import gleam/set
import gleam/string

import util

type Cell {
  Cell(r: Int, c: Int)
}

type Grid {
  Grid(w: Int, h: Int)
}

fn in_grid(c: Cell, g: Grid) -> Bool {
  c.r >= 0 && c.r < g.h && c.c >= 0 && c.c < g.w
}

fn parse_grid(txt: String) -> #(Grid, dict.Dict(Int, List(Cell))) {
  let lines =
    string.trim(txt)
    |> string.split("\n")
  let assert [head, ..] = lines
  let width = string.length(head)
  let height = list.length(lines)

  let d = {
    use dct, line, r <- list.index_fold(lines, dict.new())
    let cps =
      string.to_utf_codepoints(line)
      |> list.map(string.utf_codepoint_to_int)

    use dct, cp, c <- list.index_fold(cps, dct)
    use <- bool.guard(cp == 46, dct)

    use opt <- dict.upsert(dct, cp)
    [
      Cell(r, c),
      ..case opt {
        option.None -> []
        option.Some(cells) -> cells
      }
    ]
  }

  #(Grid(width, height), d)
}

fn antinodes(c1: Cell, c2: Cell, g: Grid) -> List(Cell) {
  let dr = c1.r - c2.r
  let dc = c1.c - c2.c

  let n1 = Cell(c1.r + dr, c1.c + dc)
  let n2 = Cell(c2.r - dr, c2.c - dc)

  case n1 |> in_grid(g) {
    False -> []
    True -> [n1]
  }
  |> list.append(case n2 |> in_grid(g) {
    False -> []
    True -> [n2]
  })
}

fn line_antinodes(c1: Cell, c2: Cell, g: Grid) -> List(Cell) {
  let #(c1, c2) = case c1.r > c2.r {
    True -> #(c2, c1)
    False -> #(c1, c2)
  }
  let dr = c2.r - c1.r
  let dc = c2.c - c1.c

  let l1 =
    iterator.unfold(c1, fn(c) {
      case !in_grid(c, g) {
        True -> iterator.Done
        False -> iterator.Next(c, Cell(c.r - dr, c.c - dc))
      }
    })
  let l2 =
    iterator.unfold(c2, fn(c) {
      case !in_grid(c, g) {
        True -> iterator.Done
        False -> iterator.Next(c, Cell(c.r + dr, c.c + dc))
      }
    })
  iterator.concat([l1, l2]) |> iterator.to_list
}

fn all_antinodes(cs: List(Cell), g: Grid) -> set.Set(Cell) {
  let cs = list.combination_pairs(cs)
  use s, #(c1, c2) <- list.fold(cs, set.new())

  line_antinodes(c1, c2, g)
  |> list.fold(s, set.insert)
}

pub fn day_main(day: util.AdventDay) {
  let assert Ok(txt) = util.read_file(day.file)
  let #(g, nodes) = parse_grid(txt)
  io.debug(g)

  let assert Ok(anti_nodes) =
    nodes
    |> dict.values
    |> list.map(all_antinodes(_, g))
    |> list.reduce(set.union)

  io.debug(set.size(anti_nodes))
  Nil
}
