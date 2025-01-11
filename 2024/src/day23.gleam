import gleam/int
import gleam/result
import gleam/bool
import gleam/io
import gleam/set
import gleam/string
import gleam/bit_array import gleam/list
import gleam/option import gleam/dict
import util

pub type Graph = dict.Dict(String, List(String))

pub fn add_edge(g: Graph, n1: String, n2: String) {
  g
    |> dict.upsert(n1, fn(opt) {
      case opt {
        option.Some(l) -> [n2, ..l]
        option.None -> [n2]
      }
    })
    |> dict.upsert(n2, fn(opt) {
      case opt {
        option.Some(l) -> [n1, ..l] option.None -> [n1]
      }
    })
} 

pub fn parse_graph(txt: String) -> Graph {
  let lines = txt |> string.trim |> string.split("\n")

  use g, line <- list.fold(lines, dict.new())
  let assert <<n1:bytes-size(2), "-", n2:bytes-size(2)>> = <<line:utf8>>
  let assert Ok(n1) = bit_array.to_string(n1)
  let assert Ok(n2) = bit_array.to_string(n2)
  add_edge(g, n1, n2)
}

pub fn three_cycles(g: Graph) -> set.Set(#(String, String, String)) {
  let step = fn(paths: List(List(String))) -> List(List(String)) {
    use p <- list.flat_map(paths)
    let assert [h, ..] = p
    let assert Ok(others) = dict.get(g, h)
    others
      |> list.filter(fn(x) {!list.contains(p, x)})
      |> list.map(fn(x) { [x, ..p] })
  }
  use s, root_node <- list.fold(dict.keys(g), set.new())
  [[root_node]] 
    |> step |> step 
    |> list.filter(fn(path) {
      let assert [h, _, t] = path
      let assert Ok(others) = dict.get(g, h)
      list.contains(others, t)
    })
    |> list.map(fn(p) {
      let assert [a, b, c] = list.sort(p, string.compare)
      #(a, b, c)
    })
    |> set.from_list
    |> set.union(s)
}

pub fn has_t_node(triple: #(String, String, String)) -> Bool {
  string.starts_with(triple.0, "t")
  || string.starts_with(triple.1, "t")
  || string.starts_with(triple.2, "t")
}

pub fn largest_connected_rec(
  g: Graph, 
  to_explore: List(#(set.Set(String), set.Set(String), String)),
  largest: set.Set(String),
) -> set.Set(String) {
  use #(cluster, possible, node), rest <- util.list_guard_empty(to_explore, largest)
  // test that the node can be added to the cluster
}

pub fn largest_connected(g: Graph) -> set.Set(String) {
  let ks = dict.keys(g)
  let p = ks |> set.from_list 
  ks
    |> list.map(fn(k) {#(set.new(), p, k)})
    |> largest_connected_rec(g, _, set.new())
}

pub fn day_main(d: util.AdventDay) {
  let assert Ok(data) = util.read_file(d.file)
  let g = parse_graph(data)
  // part 1
  g
    |> three_cycles
    |> set.to_list
    |> list.count(has_t_node)
    |> io.debug

  let assert Ok(h) = list.first(dict.keys(g))
  io.debug("Degree of "<>h<>": "<>{
    let assert Ok(n) = dict.get(g, h)
    int.to_string(list.length(n))
  })
  // part 2
  largest_connected(g)
    |> set.to_list
    |> list.sort(string.compare)
    |> string.join(",")
    |> io.debug
  Nil
}
