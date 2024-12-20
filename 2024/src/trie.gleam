import gleam/bool
import gleam/dict
import gleam/list
import gleam/result
import gleam/string
import gleam/string_tree

pub opaque type Trie {
  Node(is_term: Bool, children: dict.Dict(String, Trie))
}

pub fn new_node() -> Trie {
  Node(False, dict.new())
}

pub fn trie_insert(t: Trie, s: String) -> Trie {
  case string.pop_grapheme(s) {
    Error(_) -> Node(..t, is_term: True)
    Ok(#(g, rest)) -> {
      let c =
        t.children
        |> dict.get(g)
        |> result.unwrap(new_node())
      let ch =
        trie_insert(c, rest)
        |> dict.insert(t.children, g, _)
      Node(..t, children: ch)
    }
  }
}

fn trie_strip_rec(s: String, t: Trie, ret: List(String)) -> List(String) {
  let new_ret = case t.is_term {
    True -> [s, ..ret]
    False -> ret
  }
  case string.pop_grapheme(s) {
    Error(_) -> new_ret
    Ok(#(g, rest)) -> {
      case dict.get(t.children, g) {
        // There is no pattern that works here. Stop
        Error(_) -> new_ret
        Ok(chld) -> trie_strip_rec(rest, chld, new_ret)
      }
    }
  }
}

pub fn match(s: String, t: Trie) -> Bool {
  case string.pop_grapheme(s) {
    Error(_) -> t.is_term
    Ok(#(g, rest)) -> {
      dict.get(t.children, g)
      |> result.map(match(rest, _))
      |> result.unwrap(False)
    }
  }
}

pub fn strip(s: String, t: Trie) -> List(String) {
  trie_strip_rec(s, t, [])
}

pub fn from_list(ss: List(String)) {
  list.fold(ss, new_node(), trie_insert)
}

fn line(
  sb: string_tree.StringTree,
  ind: Int,
  rest: String,
) -> string_tree.StringTree {
  "| "
  |> string.repeat(ind)
  |> string_tree.append(sb, _)
  |> string_tree.append(rest)
  |> string_tree.append("\n")
}

fn trie_string_rec(
  sb: string_tree.StringTree,
  t: Trie,
  indent: Int,
) -> string_tree.StringTree {
  use <- bool.guard(
    dict.is_empty(t.children),
    line(sb, indent, case t.is_term {
      True -> "{T}"
      False -> "{}"
    }),
  )
  use sb, k, v <- dict.fold(t.children, sb)
  let node_name =
    k
    <> case t.is_term {
      True -> "T"
      False -> ""
    }
  line(sb, indent, node_name)
  |> trie_string_rec(v, indent + 1)
}

pub fn to_string(t: Trie) {
  trie_string_rec(string_tree.new(), t, 0) |> string_tree.to_string
}
