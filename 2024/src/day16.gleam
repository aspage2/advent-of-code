import gleam/bool
import gleam/dict.{type Dict}
import gleam/int
import gleam/io
import gleam/list
import gleam/option
import gleam/order
import gleam/pair
import gleam/result
import gleam/set.{type Set}
import gleam/string

import grid.{type Cell, type Dir, type Grid, Cell, E, N, S, W}
import util

type State {
  State(c: Cell, d: Dir)
}

pub fn opp(d: Dir) -> Dir {
  case d {
    N -> S
    S -> N
    E -> W
    W -> E
  }
}

pub fn right_turn(d: Dir) -> Dir {
  case d {
    N -> E
    E -> S
    S -> W
    W -> N
  }
}

pub fn left_turn(d: Dir) -> Dir {
  case d {
    N -> W
    W -> S
    S -> E
    E -> N
  }
}

fn next_states(s: State) -> List(#(State, Int)) {
  [#(right_turn(s.d), 1001), #(left_turn(s.d), 1001), #(s.d, 1)]
  |> list.map(pair.map_first(_, fn(d) { State(grid.move(s.c, d), d) }))
}

fn djikstra(
  g: Grid,
  end: Cell,
  vs: Set(State),
  dist: Dict(State, Int),
  up: Dict(State, List(State)),
) -> #(Dict(State, Int), Dict(State, List(State))) {
  let cmp_states = fn(d: Dict(State, Int), s1: State, s2: State) {
    let d1 = dict.get(d, s1)
    let d2 = dict.get(d, s2)
    use <- bool.guard(result.is_error(d1), s2)
    use <- bool.guard(result.is_error(d2), s1)
    let assert Ok(d1) = d1
    let assert Ok(d2) = d2
    case int.compare(d1, d2) {
      order.Lt -> s1
      _ -> s2
    }
  }

  use <- bool.guard(set.is_empty(vs), #(dist, up))

  let assert Ok(min_node) =
    vs
    |> set.to_list
    |> list.reduce(fn(a, b) { cmp_states(dist, a, b) })

  let assert Ok(node_cost) = dict.get(dist, min_node)

  let new_vs = vs |> set.delete(min_node)

  use <- bool.lazy_guard(min_node.c == end, fn() {
    djikstra(g, end, new_vs, dist, up)
  })

  let #(new_dist, new_vs, new_up) =
    next_states(min_node)
    |> list.filter(fn(s) {
      case grid.at(g, { s.0 }.c) {
        Error(_) | Ok("#") -> False
        _ -> True
      }
    })
    |> list.fold(#(dist, new_vs, up), fn(acc, st) {
      let #(d, vs, up) = acc
      let alt = st.1 + node_cost
      case dict.get(d, st.0) {
        Error(_) -> #(
          dict.insert(d, st.0, alt),
          set.insert(vs, st.0),
          dict.insert(up, st.0, [min_node]),
        )
        Ok(other_cost) if other_cost > alt -> #(
          dict.insert(d, st.0, alt),
          set.insert(vs, st.0),
          dict.insert(up, st.0, [min_node]),
        )
        Ok(other_cost) if other_cost == alt -> #(
          d,
          vs,
          dict.upsert(up, st.0, fn(opt) {
            opt
            |> option.map(fn(rest) { [min_node, ..rest] })
            |> option.unwrap([min_node])
          }),
        )
        _ -> #(dist, new_vs, up)
      }
    })
  djikstra(g, end, new_vs, new_dist, new_up)
}

fn search(
  grid: Grid,
  start: State,
  end: Cell,
) -> #(Dict(State, Int), Dict(State, List(State))) {
  djikstra(
    grid,
    end,
    set.from_list([start]),
    dict.from_list([#(start, 0)]),
    dict.new(),
  )
}

fn count_seats_rec(
  vs: Set(State),
  visited: Set(State),
  d: Dict(State, List(State)),
) -> Int {
  use <- bool.lazy_guard(set.is_empty(vs), fn() {
    set.to_list(visited)
    |> list.map(fn(x) { x.c })
    |> list.unique
    |> list.length
  })

  let new_vs =
    vs
    |> set.to_list
    |> list.flat_map(fn(c) { dict.get(d, c) |> result.unwrap([]) })
    |> set.from_list
    |> set.difference(visited)
  count_seats_rec(new_vs, visited |> set.union(new_vs), d)
}

fn count_seats(st: State, d: Dict(State, List(State))) -> Int {
  count_seats_rec(set.from_list([st]), set.from_list([st]), d)
}

fn get_info(g: Grid) -> #(Cell, Cell) {
  let init = #(Cell(0, 0), Cell(0, 0))
  use pair, r <- list.fold(list.range(0, g.height - 1), init)
  use pair, cl <- list.fold(list.range(0, g.width - 1), pair)
  let c = Cell(r, cl)
  case grid.at(g, c) {
    Error(_) -> panic
    Ok("S") -> #(c, pair.1)
    Ok("E") -> #(pair.0, c)
    _ -> pair
  }
}

pub fn day_main(day: util.AdventDay) -> Nil {
  let assert Ok(txt) = util.read_file(day.file) |> result.map(string.trim)
  let assert Ok(g) = grid.parse_grid(txt)
  let #(st, end) = get_info(g)
  io.debug(#(st, end))
  io.debug(#(g.width, g.height))
  let #(dst, up) = search(g, State(st, E), end)
  let assert Ok(#(min_k, cost)) =
    dict.filter(dst, fn(k, _) { k.c == end })
    |> dict.to_list
    |> list.reduce(fn(a, b) {
      case a.1 < b.1 {
        True -> a
        False -> b
      }
    })
  io.debug(cost)
  io.debug(min_k)
  io.debug(count_seats(min_k, up))
  Nil
}
