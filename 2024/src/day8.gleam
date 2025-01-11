import gleam/yielder
import gleam/option
import gleam/dict
import gleam/io
import gleam/list
import gleam/set
import gleam/string

import util
import grid.{type Grid, type Cell, Cell}

fn in_grid(c: Cell, g: Grid) -> Bool {
  grid.contains(g, c)
}

fn parse_grid(txt: String) -> #(Grid, dict.Dict(Int, List(Cell))) {
  let assert Ok(g) = grid.parse_grid(txt)

  let cells = {
    use r <- list.flat_map(list.range(0, g.height - 1))
    use c <- list.map(list.range(0, g.width - 1))
    Cell(r, c)
  }

  let lookup = list.fold(cells, dict.new(), fn(dct, c) {
    case grid.at(g, c) {
    Ok(".") -> dct
    Ok(x) -> {
      let assert [h] = string.to_utf_codepoints(x)
      use opt <- dict.upsert(dct, h |> string.utf_codepoint_to_int)
      case opt {
      option.None -> [c]
      option.Some(l) -> [c, ..l]
      }
    }
    Error(_) -> panic
    }
  })
  #(g, lookup)
}

pub fn antinodes(c1: Cell, c2: Cell, g: Grid) -> List(Cell) {
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
    yielder.unfold(c1, fn(c) {
      case !in_grid(c, g) {
        True -> yielder.Done
        False -> yielder.Next(c, Cell(c.r - dr, c.c - dc))
      }
    })
  let l2 =
    yielder.unfold(c2, fn(c) {
      case !in_grid(c, g) {
        True -> yielder.Done
        False -> yielder.Next(c, Cell(c.r + dr, c.c + dc))
      }
    })
  yielder.concat([l1, l2]) |> yielder.to_list
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
