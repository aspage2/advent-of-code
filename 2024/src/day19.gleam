import gleam/bool
import gleam/dict
import gleam/int
import gleam/io
import gleam/list
import gleam/string

import counter.{type Counter}
import trie.{type Trie}
import util

fn parse(txt: String) -> #(List(String), List(String)) {
  let lines =
    txt
    |> string.trim
    |> string.split("\n")

  let assert [textiles, _, ..rest] = lines
  #(string.split(textiles, ", "), rest)
}

fn possible_rec(opts: Counter(String), t: Trie, i: Int) -> Int {
  use <- bool.guard(dict.is_empty(opts), i)
  let new_opts =
    opts
    |> counter.multiply(trie.strip(_, t))
  let num_done =
    new_opts
    |> counter.get_count("")
  possible_rec(new_opts, t, i + num_done)
}

fn possible(s: String, t: Trie) -> Int {
  possible_rec(dict.from_list([#(s, 1)]), t, 0)
}

pub fn day_main(day: util.AdventDay) {
  let assert Ok(txt) = util.read_file(day.file)

  let #(a, b) = parse(txt)
  let t = trie.from_list(a)

  b
  |> list.map(fn(x) { possible(x, t) })
  |> int.sum
  |> io.debug

  Nil
}
