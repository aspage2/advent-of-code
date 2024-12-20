import gleam/dict
import gleam/io

import counter

pub fn run() -> Nil {
  [#(3, 2), #(4, 1), #(11, 13), #(8, 1)]
  |> counter.from_list
  |> counter.map(fn(x) { x % 8 })
  |> dict.to_list
  |> io.debug

  Nil
}
