import gleam/bit_array
import gleam/dict
import gleam/int
import gleam/list
import gleam/option
import gleam/result
import gleam/string

pub type Atom

@external(erlang, "init", "get_plain_arguments")
fn erl_get_plain_arguments() -> List(List(UtfCodepoint))

pub fn argv() -> List(String) {
  erl_get_plain_arguments()
  |> list.map(string.from_utf_codepoints)
}

pub type AdventDay {
  AdventDay(day: Int, part: Int, file: String)
}

fn parse_args_rec(
  day: AdventDay,
  args: List(String),
) -> Result(AdventDay, String) {
  case args {
    [] -> Ok(day)
    [x] -> Error("singular arg " <> x <> ", must use a flag")
    [flg, val, ..rest] -> {
      use new_day <- result.try(case flg {
        "--day" ->
          int.parse(val)
          |> result.replace_error("Day must be an integer")
          |> result.map(fn(i) { AdventDay(..day, day: i) })
        "--part" ->
          int.parse(val)
          |> result.replace_error("Part must be an integer")
          |> result.then(fn(i) {
            case i >= 1 && i <= 2 {
              True -> Ok(i)
              False -> Error("Part must be 1 or 2")
            }
          })
          |> result.map(fn(i) { AdventDay(..day, part: i) })
        "--file" -> Ok(AdventDay(..day, file: val))

        x -> Error("Not a flag: " <> x)
      })
      parse_args_rec(new_day, rest)
    }
  }
}

pub fn parse_args(args: List(String)) -> Result(AdventDay, String) {
  parse_args_rec(AdventDay(1, 1, "data/day1.txt"), args)
}

@external(erlang, "file", "read_file")
fn erl_read_file(fname: String) -> Result(BitArray, Atom)

@external(erlang, "erlang", "atom_to_list")
fn erl_atom_to_list(a: Atom) -> List(UtfCodepoint)

pub fn read_file_raw(fname: String) -> Result(BitArray, String) {
  erl_read_file(fname)
  |> result.map_error(fn(a) {
    erl_atom_to_list(a) |> string.from_utf_codepoints
  })
}

pub fn read_file(fname: String) -> Result(String, String) {
  use data <- result.try(read_file_raw(fname))
  bit_array.to_string(data)
  |> result.replace_error("not valid utf8")
}
