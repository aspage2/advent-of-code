import gleam/bool
import gleam/int
import gleam/io
import gleam/list
import gleam/order
import gleam/result
import gleam/string
import gleam/string_tree
import util

pub type Block {
  Block(id: Int, size: Int)
}

fn take_file(acc: Int, graphemes: List(String)) -> #(List(Block), List(Int)) {
  case graphemes {
    [] -> #([], [])
    [b, ..rest] -> {
      let assert Ok(num) = int.parse(b)
      let #(bs, ss) = take_space(acc + 1, rest)
      #([Block(acc, num), ..bs], ss)
    }
  }
}

fn take_space(acc: Int, graphemes: List(String)) -> #(List(Block), List(Int)) {
  case graphemes {
    [] -> #([], [])
    [b, ..rest] -> {
      let assert Ok(num) = int.parse(b)
      let #(bs, ss) = take_file(acc, rest)
      #(bs, [num, ..ss])
    }
  }
}

pub fn parse_disk_map(txt: String) -> #(List(Block), List(Int)) {
  let gs =
    txt
    |> string.trim
    |> string.to_graphemes
  take_file(0, gs)
}

pub fn compact_disk_map_rec(
  map: List(Block),
  rev: List(Block),
  spc: List(Int),
  partial: Bool,
) -> List(Block) {
  let assert [mh, ..m_rest] = map
  let #(ret, m_rest) = case partial {
    True -> #([], map)
    False -> #([mh], m_rest)
  }
  let assert [rh, ..r_rest] = rev
  let assert [sh, ..s_rest] = spc

  use <- bool.guard(mh.id == rh.id, [rh])
  use <- bool.guard(mh.id > rh.id, [])

  ret
  |> list.append(case int.compare(rh.size, sh) {
    order.Lt -> [
      rh,
      ..compact_disk_map_rec(m_rest, r_rest, [sh - rh.size, ..s_rest], True)
    ]
    order.Eq -> [rh, ..compact_disk_map_rec(m_rest, r_rest, s_rest, False)]
    order.Gt -> [
      Block(..rh, size: sh),
      ..compact_disk_map_rec(
        m_rest,
        [Block(..rh, size: rh.size - sh), ..r_rest],
        s_rest,
        False,
      )
    ]
  })
}

pub fn compact_disk(bs: List(Block), spc: List(Int)) {
  compact_disk_map_rec(bs, list.reverse(bs), spc, False)
}

pub fn checksum(bs: List(Block)) -> Int {
  let #(_, ret) = {
    use #(i, acc), b <- list.fold(bs, #(0, 0))
    use <- bool.guard(b.size == 0, #(i, acc))
    let sum = list.range(i, i + b.size - 1) |> int.sum()
    #(i + b.size, acc + { sum * b.id })
  }
  ret
}

pub fn to_string_rec(
  sb: string_tree.StringTree,
  bs: List(Block),
  spc: List(Int),
) -> String {
  case bs {
    [] -> string_tree.to_string(sb)
    [h, ..rest] -> {
      let sb =
        h.id
        |> int.to_string
        |> string.repeat(h.size)
        |> string_tree.append(sb, _)

      let #(sb, spc) = case spc {
        [] -> #(sb, spc)
        [sh, ..srest] -> #(
          string.repeat(".", sh)
            |> string_tree.append(sb, _),
          srest,
        )
      }
      to_string_rec(sb, rest, spc)
    }
  }
}

pub fn to_string(bs: List(Block), spc: List(Int)) -> String {
  to_string_rec(string_tree.new(), bs, spc)
}

pub fn day_main(day: util.AdventDay) {
  let assert Ok(txt) = util.read_file(day.file)
  let #(bs, spc) = parse_disk_map(txt)
  let res = compact_disk(bs, spc)
  io.debug(to_string(bs, spc))
  io.debug(to_string(res, []))
  io.debug("==================")
  io.debug(checksum(res))
  Nil
}
