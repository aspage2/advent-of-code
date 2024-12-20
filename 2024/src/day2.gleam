import gleam/bool
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order
import gleam/result
import gleam/string

import util

/// True if the pair has an abs difference between 1 and 3,
/// and their direction is the given one
fn pair_is_safe(x: Int, y: Int, in_direction: order.Order) -> Bool {
  let dir = int.compare(x, y)
  let diff = int.absolute_value(x - y)
  diff >= 1 && diff <= 3 && in_direction == dir
}

fn is_safe_rec_1(report: List(Int), in_direction: order.Order) -> Bool {
  case report {
    [] | [_] -> True
    [x, y, ..rest] ->
      pair_is_safe(x, y, in_direction)
      && is_safe_rec_1([y, ..rest], in_direction)
  }
}

fn is_safe_1(report: List(Int)) -> Bool {
  is_safe_rec_1(report, order.Lt) || is_safe_rec_1(report, order.Gt)
}

fn is_safe_rec(
  report: List(Int),
  in_direction: order.Order,
  fixed_once: Bool,
  prev: Option(Int),
) -> Bool {
  case report {
    [] | [_] -> True
    [x, y, ..rest] -> {
      use <- bool.lazy_guard(pair_is_safe(x, y, in_direction), fn() {
        is_safe_rec([y, ..rest], in_direction, fixed_once, Some(x))
      })

      use <- bool.guard(fixed_once, False)

      let #(o1, o2) =
        prev
        |> option.map(fn(p) { #([p, y, ..rest], [p, x, ..rest]) })
        |> option.unwrap(#([y, ..rest], [x, ..rest]))

      is_safe_rec(o1, in_direction, True, None)
      || is_safe_rec(o2, in_direction, True, None)
    }
  }
}

fn is_safe(report: List(Int)) -> Bool {
  // Don't infer direction during the recursive step. 
  // rather, check both.
  is_safe_rec(report, order.Lt, False, None)
  || is_safe_rec(report, order.Gt, False, None)
}

pub fn day_main(day: util.AdventDay) {
  let assert Ok(lines) =
    util.read_file(day.file)
    |> result.map(string.trim)
    |> result.map(string.split(_, "\n"))

  let assert Ok(reports) =
    lines
    |> list.map(fn(line) {
      string.split(line, " ")
      |> list.map(int.parse)
      |> result.all
    })
    |> result.all

  io.println("Part1")
  io.debug(list.count(reports, is_safe_1))
  io.println("Part2")
  io.debug(list.count(reports, is_safe))
  Nil
}
