import gleam/bool
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string

import util

fn parse_line(line: String) -> #(Int, List(Int)) {
  let assert Ok(#(fst, rest)) = string.split_once(line, ": ")

  let assert Ok(res) = {
    use fst <- result.try(int.parse(fst))

    let rest_res =
      rest
      |> string.trim
      |> string.split(" ")
      |> list.map(int.parse)
      |> result.all

    use rest <- result.try(rest_res)

    Ok(#(fst, rest))
  }
  res
}

fn fix_eq_rec(
  acc: Int,
  left: Int,
  operands: List(Int),
  operators: List(fn(Int, Int) -> Int),
) -> Bool {
  case operands {
    [] -> acc == left
    [x, ..rest] -> {
      use <- bool.guard(acc > left, False)
      use f <- list.any(operators)
      fix_eq_rec(f(acc, x), left, rest, operators)
    }
  }
}

fn fix_eq(
  left: Int,
  operands: List(Int),
  operators: List(fn(Int, Int) -> Int),
) -> Bool {
  //	let tot = int.sum(operands)
  //	use <- bool.guard(tot > left, False)
  //	use <- bool.guard(tot == left, True)
  case operands {
    [] -> False
    [x, ..rest] -> fix_eq_rec(x, left, rest, operators)
  }
}

pub fn day_main(day: util.AdventDay) {
  let assert Ok(lines) =
    util.read_file(day.file)
    |> result.map(string.trim)
    |> result.map(string.split(_, "\n"))

  let ops = case day.part {
    1 -> [fn(a, b) { a + b }, fn(a, b) { a * b }]

    2 -> [
      fn(a, b) { a + b },
      fn(a, b) { a * b },
      fn(a, b) {
        let assert Ok(ret) = int.parse(int.to_string(a) <> int.to_string(b))
        ret
      },
    ]
    _ -> panic
  }

  lines
  |> list.map(parse_line)
  |> list.filter(fn(pair) { fix_eq(pair.0, pair.1, ops) })
  |> list.map(fn(pair) { pair.0 })
  |> int.sum()
  |> io.debug

  Nil
}
