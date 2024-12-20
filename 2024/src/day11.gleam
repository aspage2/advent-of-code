import gleam/bit_array
import gleam/bool
import gleam/dict
import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string

import counter
import util

const main_input = "2701 64945 0 9959979 93 781524 620 1"

const example_input = "125 17"

@external(erlang, "math", "log10")
fn erl_log10(x: Int) -> Float

fn num_digits(x: Int) -> Int {
  let x =
    erl_log10(x)
    |> float.floor
    |> float.round
  x + 1
}

fn rock_next(i: Int) -> List(Int) {
  use <- bool.guard(i == 0, [1])
  let num_digits = num_digits(i)
  case num_digits % 2 {
    0 -> {
      let nover2 = num_digits / 2
      let s = int.to_string(i)
      let assert <<fst:bytes-size(nover2), snd:bytes>> = <<s:utf8>>
      let assert Ok(x) =
        result.all([
          bit_array.to_string(fst) |> result.then(int.parse),
          bit_array.to_string(snd) |> result.then(int.parse),
        ])
      x
    }
    _ -> [2024 * i]
  }
}

fn part(in: String) -> Nil {
  let assert Ok(data) =
    in
    |> string.split(" ")
    |> list.map(int.parse)
    |> result.all

  let c = data |> counter.from_keys
  let res =
    list.fold(list.range(1, 25), c, fn(acc, _) {
      counter.multiply(acc, rock_next)
    })
  io.println("25 iterations: " <> int.to_string(counter.total(res)))
  let res =
    list.fold(list.range(1, 50), res, fn(acc, _) {
      counter.multiply(acc, rock_next)
    })
  io.println("75 iterations: " <> int.to_string(dict.values(res) |> int.sum))
}

pub fn day_main(_: util.AdventDay) {
  io.println("example input: " <> example_input)
  part(example_input)
  io.println("-----------")
  io.println("main input: " <> main_input)
  part(main_input)

  Nil
}
