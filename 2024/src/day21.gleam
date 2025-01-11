import gleam/result
import gleam/io
import gleam/int
import gleam/string
import gleam/bool
import gleam/list
import util
import grid.{type Cell, Cell}

fn numpad_by_symbol(sym: String) -> Cell {
  case sym {
    "7"-> Cell(0, 0)
    "8"-> Cell(0, 1)
    "9"-> Cell(0, 2)
    "4"-> Cell(1, 0)
    "5"-> Cell(1, 1)
    "6"-> Cell(1, 2)
    "1"-> Cell(2, 0)
    "2"-> Cell(2, 1)
    "3"-> Cell(2, 2)
    "0"-> Cell(3, 1)
    "A"-> Cell(3, 2)
    _ -> panic
  }
}

pub fn numpad_by_cell(cell: Cell) -> Result(String, Nil) {
  case cell {
    Cell(0, 0) -> Ok("7")
    Cell(0, 1) -> Ok("8")
    Cell(0, 2) -> Ok("9")
    Cell(1, 0) -> Ok("4")
    Cell(1, 1) -> Ok("5")
    Cell(1, 2) -> Ok("6")
    Cell(2, 0) -> Ok("1")
    Cell(2, 1) -> Ok("2")
    Cell(2, 2) -> Ok("3")
    Cell(3, 1) -> Ok("0")
    Cell(3, 2) -> Ok("A")
    _ -> Error(Nil)
  }
}

fn dpad_by_symbol(sym: String) -> Cell {
  case sym {
    "^" -> Cell(0, 1)
    "v" -> Cell(1, 1)
    "<" -> Cell(1, 0)
    ">" -> Cell(1, 2)
    "A" -> Cell(0, 2)
    _ -> panic
  }
}

pub fn dpad_by_cell(cell: Cell) -> Result(String, Nil) {
  case cell {
    Cell(0, 1) -> Ok("^")
    Cell(1, 1) -> Ok("v")
    Cell(1, 0) -> Ok("<")
    Cell(1, 2) -> Ok(">")
    Cell(0, 2) -> Ok("A")
    _ -> Error(Nil)
  }
}

fn h_dir(x: Int) -> String { 
  case x < 0 {
    True -> "<"
    False -> ">"
  }
}

fn v_dir(x: Int) -> String { 
  case x < 0 {
    True -> "^"
    False -> "v"
  }
}

fn get_paths_rec(
  acc: List(String), 
  dirs: #(Int, Int),
  path: List(String), 
  curr: Cell, 
  end: Cell,
  sym_to_cell: fn(String) -> Cell, 
  cell_to_sym: fn(Cell) -> Result(String, Nil)
) -> List(String) {
  use <- bool.lazy_guard(curr==end, fn() {
    let new_path = ["A", ..path] |> list.reverse
    [new_path |> string.join(""), ..acc]
  })
  case cell_to_sym(curr) {
  Error(_) -> acc
  Ok(_) -> {
    let acc = case dirs.0 {
      0 -> acc
      _ -> get_paths_rec(
        acc,
        dirs,
        [v_dir(dirs.0), ..path], 
        Cell(..curr, r: curr.r + dirs.0),
        end,
        sym_to_cell,
        cell_to_sym,
      )
    }
    case dirs.1 {
      0 -> acc
      _ -> get_paths_rec(
        acc,
        dirs,
        [h_dir(dirs.1), ..path], 
        Cell(..curr, c: curr.c + dirs.1),
        end,
        sym_to_cell,
        cell_to_sym,
      )
    }}
  }
}

fn get_paths_numpad(
  st: Cell, 
  end: Cell, 
) -> List(String) {
  let h_path = case end.c - st.c {
    0 -> ""
    h_dist -> h_dir(h_dist)
      |> string.repeat(int.absolute_value(h_dist))
  }
  let v_path = case end.r - st.r {
    0 -> ""
    dist -> h_dir(dist)
      |> string.repeat(int.absolute_value(dist))
  }

  use <- bool.guard(st.r == 3 && end.c == 0, [v_path <> h_path])
  use <- bool.guard(end.r == 3 && st.c == 0, [h_path <> v_path])

  [h_path <> v_path, v_path <> h_path]
}

fn get_paths_dpad(
  st: Cell, 
  end: Cell, 
) -> List(String) {
  let h_path = case end.c - st.c {
    0 -> ""
    h_dist -> h_dir(h_dist)
      |> string.repeat(int.absolute_value(h_dist))
  }
  let v_path = case end.r - st.r {
    0 -> ""
    dist -> v_dir(dist)
      |> string.repeat(int.absolute_value(dist))
  }

  use <- bool.guard(st.r == 0 && end.c == 0, [v_path <> h_path])
  use <- bool.guard(end.r == 0 && st.c == 0, [h_path <> v_path])

  [h_path <> v_path, v_path <> h_path]
}

fn get_all_walks(
  seq: String, 
  sym_to_cell: fn(String) -> Cell, 
  get_paths: fn(Cell, Cell) -> List(String),
) -> List(String) {
  // Start the robot on "A"
  let seq = ["A", ..string.to_graphemes(seq)]
  let pairs = seq 
    |> list.map(sym_to_cell)
    |> list.window_by_2
  let paths = {
    use acc, #(st, end) <- list.fold(pairs, [""])
    let paths = get_paths(st, end)
    use pref <- list.flat_map(acc)
    use path <- list.map(paths)
    pref <> path
  }
  let assert Ok(min_len) = paths
    |> list.map(string.length)
    |> list.reduce(int.min)
  paths
    |> list.filter(fn(x) {string.length(x) == min_len})
}

pub fn complexity(seq: String) -> Int {
    let assert Ok(shortest_seq) = 
    get_all_walks(seq, numpad_by_symbol, get_paths_numpad)
    |> list.flat_map(get_all_walks(_, dpad_by_symbol, get_paths_dpad))
    |> list.flat_map(get_all_walks(_, dpad_by_symbol, get_paths_dpad))
    |> list.map(string.length)
    |> list.reduce(int.min)

    let assert Ok(num) = seq
      |> string.slice(0, 3)
      |> int.parse

    num * shortest_seq
}

pub fn day_main(day: util.AdventDay) -> Nil {
  let assert Ok(data) = util.read_file(day.file)
  |> result.map(string.trim)
  |> result.map(string.split(_, "\n"))
  data
    |> list.map(complexity)
    |> int.sum
    |> io.debug
  Nil
}
