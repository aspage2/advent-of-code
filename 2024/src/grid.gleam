import gleam/bit_array
import gleam/bool
import gleam/bytes_tree
import gleam/list
import gleam/string

/// A Grid is a 2D rectangular plane made up
/// of whole-number cells. It uses a bitarray
/// as its underlying datastructure.
pub type Grid {
  Grid(data: BitArray, width: Int, height: Int)
}

/// A Cell is the location within a Grid.
pub type Cell {
  Cell(r: Int, c: Int)
}

pub type Dir {
  N
  S
  E
  W
}

pub fn move(c: Cell, d: Dir) -> Cell {
  let Cell(r, c) = c
  case d {
    N -> Cell(r - 1, c)
    S -> Cell(r + 1, c)
    E -> Cell(r, c + 1)
    W -> Cell(r, c - 1)
  }
}

// Convert the cell to the bit_array index
fn ind(g: Grid, c: Cell) -> Int {
  g.width * c.r + c.c
}

// View the element at the given location in the
// grid. Returns Error(Nil) if the cell is out of bounds.
pub fn at(g: Grid, c: Cell) -> Result(String, Nil) {
  let ind = ind(g, c)
  use <- bool.guard(!contains(g, c), Error(Nil))
  case g.data {
    <<_:bytes-size(ind), ret, _:bytes>> -> bit_array.to_string(<<ret>>)
    _ -> Error(Nil)
  }
}

// True iff the cell is within the bounds of g.
pub fn contains(g: Grid, c: Cell) -> Bool {
  c.c >= 0 && c.c < g.width && c.r >= 0 && c.r < g.height
}

// Parse a grid from a text string. Grid rows are expected
// to be separated by a newline character.
pub fn parse_grid(txt: String) -> Result(Grid, Nil) {
  let lines = string.split(txt |> string.trim, "\n")
  case lines {
    [] -> Error(Nil)
    [head, ..] -> {
      let data =
        lines
        |> list.map(bit_array.from_string)
        |> bytes_tree.concat_bit_arrays
        |> bytes_tree.to_bit_array

      Ok(Grid(data, string.length(head), list.length(lines)))
    }
  }
}

// Return a list of neighbors in the cardinal directions
// of this Cell.
pub fn cardinal_neighbors(c: Cell) -> List(Cell) {
  [
    Cell(c.r - 1, c.c),
    Cell(c.r + 1, c.c),
    Cell(c.r, c.c - 1),
    Cell(c.r, c.c + 1),
  ]
}

pub fn grid_cells(g: Grid) -> List(Cell) {
  use r <- list.flat_map(list.range(0, g.height - 1))
  use c <- list.map(list.range(0, g.width - 1))
  Cell(r, c)
}
