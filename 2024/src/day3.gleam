import gleam/bool
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import util

fn take_mul_op(data: String) -> #(Result(#(Int, Int), Nil), String) {
  use <- bool.guard(!string.starts_with(data, "mul("), #(Error(Nil), data))
  let rest = string.drop_start(data, 4)

  let res = {
    use #(first, rest) <- result.try(string.split_once(rest, ","))
    use #(second, rest) <- result.try(string.split_once(rest, ")"))

    use first <- result.try(int.parse(first))
    use second <- result.try(int.parse(second))

    Ok(#(#(first, second), rest))
  }

  case res {
    Ok(#(p1, rest)) -> #(Ok(p1), rest)
    Error(_) -> #(Error(Nil), data)
  }
}

fn traverse_part1(data: String) -> List(#(Int, Int)) {
  use <- bool.guard(data == "", [])
  let #(maybe_pair, rest) = take_mul_op(data)

  case maybe_pair {
    Ok(p) -> [p, ..traverse_part1(rest)]
    Error(_) -> traverse_part1(string.drop_start(data, 1))
  }
}

fn traverse_part2(
  acc: List(#(Int, Int)),
  data: String,
  en: Bool,
) -> List(#(Int, Int)) {
  use <- bool.guard(data == "", acc)
  case en {
    True -> {
      use <- bool.lazy_guard(string.starts_with(data, "don't()"), fn() {
        traverse_part2(acc, string.drop_start(data, 7), False)
      })
      let #(maybe_pair, rest) = take_mul_op(data)
      case maybe_pair {
        Ok(p) -> traverse_part2([p, ..acc], rest, True)
        Error(_) -> traverse_part2(acc, string.drop_start(data, 1), True)
      }
    }
    False -> {
      case string.starts_with(data, "do()") {
        True -> traverse_part2(acc, string.drop_start(data, 4), True)
        False -> traverse_part2(acc, string.drop_start(data, 1), False)
      }
    }
  }
}

pub fn day_main(day: util.AdventDay) {
  let assert Ok(txt) = util.read_file(day.file) |> result.map(string.trim)

  io.debug(string.length(txt))

  traverse_part2([], txt, True)
  |> list.map(fn(p) { p.0 * p.1 })
  |> int.sum()
  |> io.debug()

  Nil
}
