import gleam/bool
import gleam/dict
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/set
import grid.{type Cell, type Grid, Cell}
import util

pub type Dir {
  Up
  Left
  Down
  Right
}

pub type Region =
  set.Set(Cell)

pub type Edge =
  #(Cell, Dir)

pub type Perimiter =
  List(Edge)

fn num_sides(es: Perimiter) -> Int {
  let up_cnt =
    es
    |> list.filter(fn(p) { p.1 == Up || p.1 == Down })
    |> list.group(fn(p) { #({ p.0 }.r, p.1) })
    |> dict.values
    |> list.map(fn(ps) {
      list.map(ps, fn(p) { { p.0 }.c })
      |> count_contiguous
    })
    |> int.sum

  let v_cnt =
    es
    |> list.filter(fn(p) { p.1 == Left || p.1 == Right })
    |> list.group(fn(p) { #({ p.0 }.c, p.1) })
    |> dict.values
    |> list.map(fn(ps) {
      list.map(ps, fn(p) { { p.0 }.r })
      |> count_contiguous
    })
    |> int.sum
  up_cnt + v_cnt
}

fn region_rec(g: Grid, letter: String, acc: Region, vs: Region) -> Region {
  use <- bool.guard(set.is_empty(vs), acc)
  let next =
    vs
    |> set.to_list
    |> list.flat_map(grid.cardinal_neighbors)
    |> list.filter(fn(x) {
      case grid.at(g, x) {
        Ok(s) -> s == letter
        Error(_) -> False
      }
    })
    |> set.from_list
    |> set.difference(acc)

  region_rec(g, letter, set.union(acc, next), next)
}

pub fn across_matches(g: Grid, e: Edge) -> Bool {
  let #(c, d) = e
  let assert Ok(s) = grid.at(g, c)
  let other = case d {
    Up -> Cell(c.r - 1, c.c)
    Down -> Cell(c.r + 1, c.c)
    Left -> Cell(c.r, c.c - 1)
    Right -> Cell(c.r, c.c + 1)
  }
  case grid.at(g, other) {
    Error(_) -> False
    Ok(x) -> s == x
  }
}

pub fn region(g: Grid, st: Cell) -> #(Region, Perimiter) {
  let assert Ok(s) = grid.at(g, st)
  let acc = set.from_list([st])
  let r = region_rec(g, s, acc, acc)

  let ps =
    r
    |> set.to_list
    |> list.flat_map(fn(x) { [#(x, Up), #(x, Down), #(x, Left), #(x, Right)] })
    |> list.filter(fn(e) { !across_matches(g, e) })

  #(r, ps)
}

fn guard_head(ls: List(a), def: b, f: fn(a, List(a)) -> b) -> b {
  case ls {
    [] -> def
    [head, ..rest] -> f(head, rest)
  }
}

fn all_regions_rec(
  g: Grid,
  gs: List(Cell),
  d: List(#(Region, Perimiter)),
) -> List(#(Region, Perimiter)) {
  use h, _ <- guard_head(gs, d)
  let r = region(g, h)
  all_regions_rec(
    g,
    gs
      |> set.from_list
      |> set.difference(r.0)
      |> set.to_list,
    [r, ..d],
  )
}

fn count_contiguous(l: List(Int)) -> Int {
  let l = list.sort(l, int.compare)
  use h, rest <- guard_head(l, 0)
  {
    use #(curr, tot), x <- list.fold(rest, #(h, 1))

    case curr + 1 == x {
      True -> #(x, tot)
      False -> #(x, tot + 1)
    }
  }.1
}

pub fn all_regions(g: Grid) -> List(#(Region, Perimiter)) {
  all_regions_rec(g, grid.grid_cells(g), [])
}

pub fn day_main(day: util.AdventDay) {
  let assert Ok(g) =
    util.read_file(day.file)
    |> result.replace_error(Nil)
    |> result.then(grid.parse_grid)

  let ar = all_regions(g)
  ar
  |> list.map(fn(r) { set.size(r.0) * list.length(r.1) })
  |> int.sum
  |> io.debug

  ar
  |> list.map(fn(p) { num_sides(p.1) * set.size(p.0) })
  |> int.sum
  |> io.debug

  Nil
}
