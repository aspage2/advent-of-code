import day14
import gleam/pair
import gleam/set
import gleam/order
import gleam/dict
import counter
import gleam/yielder
import gleam/io
import gleam/list
import gleam/string
import gleam/result
import gleam/int
import util

fn next_secret(s: Int) -> Int {
  let s = s
    |> int.bitwise_shift_left(6)
    |> int.bitwise_exclusive_or(s)
    |> int.bitwise_and(0xffffff)
  let s = s
    |> int.bitwise_shift_right(5)
    |> int.bitwise_exclusive_or(s)
    |> int.bitwise_and(0xffffff)
  s
    |> int.bitwise_shift_left(11)
    |> int.bitwise_exclusive_or(s)
    |> int.bitwise_and(0xffffff)
}

pub type Pattern = #(Int, Int, Int, Int)

pub fn process_seed(
  ctr: counter.Counter(Pattern), s: Int,
) -> counter.Counter(Pattern) {
  let prices = s
    |> yielder.iterate(next_secret)
    |> yielder.take(2000)
    |> yielder.map(fn(x) {x % 10})
    |> yielder.to_list

  let diffs = prices
    |> list.window_by_2
    |> list.map(fn(p) { p.1 - p.0 })

  let fours = diffs
    |> list.zip(list.drop(prices, 1))
    |> list.window(4)

  use <- day14.defer(pair.first)
  use #(ctr, v), l <- list.fold(fours, #(ctr, set.new()))
  let assert [#(a, _), #(b, _), #(c, _), #(d, p)] = l
  let f = #(a, b, c, d)
  let ctr = case set.contains(v, f) {
    True -> ctr
    _ -> counter.inc_by(ctr, f, p)
  }
  #(ctr, set.insert(v, f))
}

pub fn count_all(seeds: List(Int)) -> counter.Counter(Pattern) {
  list.fold(seeds, counter.new(), process_seed)
}

pub fn day_main(day: util.AdventDay) {
  let assert Ok(seeds) = {
    let res = util.read_file(day.file) 
      |> result.replace_error(Nil)
    use data <- result.try(res)
    data
      |> string.trim 
      |> string.split("\n")
      |> list.map(int.parse)
      |> result.all
  }

  seeds
    |> list.map(fn(s) {
      let assert Ok(ret) = yielder.iterate(s, next_secret)
        |> yielder.drop(2000)
        |> yielder.first
        ret
    })
    |> int.sum

  count_all(seeds)
    |> dict.to_list
    |> list.fold(#(#(0,0,0,0), 0), fn(mx, p) {
    case int.compare(p.1, mx.1) {
    order.Gt -> p
    _ -> mx
    }})
    |> io.debug
  Nil
}
