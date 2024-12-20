import gleam/bool
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string

import util
import zipper.{type Zipper}

type Block {
  File(id: Int, size: Int)
  Space(size: Int)
}

// Apply the first function to the result from the second.
// Helpful for transforming outputs from lower use-blocks
fn defer(f: fn(a) -> b, blk: fn() -> a) -> b {
  f(blk())
}

fn parse_summary(txt: String) -> List(Block) {
  use <- defer(fn(x) {
    let #(a, _, _) = x
    a |> list.reverse
  })
  use #(acc, is_file, id), g <- list.fold(string.to_graphemes(txt), #(
    [],
    True,
    0,
  ))
  let assert Ok(size) = int.parse(g)
  case is_file {
    True -> #([File(id, size), ..acc], False, id + 1)
    False -> #([Space(size), ..acc], True, id)
  }
}

const b64_graphemes = <<
  "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz+/",
>>

fn b64encode(cp: Int) -> String {
  let assert <<_:bytes-size(cp), ret:utf8_codepoint, _:bytes>> = b64_graphemes
  string.from_utf_codepoints([ret])
}

fn to_string(l: List(Block)) -> String {
  l
  |> list.map(fn(b) {
    case b {
      File(id, size) -> b64encode(id) |> string.repeat(size)
      Space(size) -> string.repeat(".", size)
    }
  })
  |> string.concat
}

// Insert from the end. Return how much of the file is left.
fn insert_at_end(l: List(Block), id: Int, file_size: Int) -> #(List(Block), Int) {
  case l {
    [] -> #([], file_size)
    [h, ..rest] -> {
      let #(l, file_size) = insert_at_end(rest, id, file_size)
      use <- bool.guard(file_size == 0, #([h, ..l], 0))
      case h {
        Space(space_size) if space_size > file_size -> #(
          [Space(space_size - file_size), File(id, file_size), ..l],
          0,
        )
        Space(space_size) if space_size > 0 -> #(
          [File(id, space_size), ..l],
          file_size - space_size,
        )
        _ -> #([h, ..l], file_size)
      }
    }
  }
}

fn insert_no_frag(
  l: List(Block),
  id: Int,
  file_size: Int,
) -> #(List(Block), Int) {
  case l {
    [] -> #([], file_size)
    [h, ..rest] -> {
      let #(l, file_size) = insert_no_frag(rest, id, file_size)
      use <- bool.guard(file_size == 0, #([h, ..l], 0))
      case h {
        Space(space_size) if space_size > file_size -> #(
          [Space(space_size - file_size), File(id, file_size), ..l],
          0,
        )
        Space(space_size) if space_size == file_size -> #(
          [File(id, file_size), ..l],
          0,
        )
        _ -> #([h, ..l], file_size)
      }
    }
  }
}

type Inserter =
  fn(List(Block), Int, Int) -> #(List(Block), Int)

fn compress_rec(
  z: Zipper(Block),
  last_id: Int,
  insert: Inserter,
) -> Zipper(Block) {
  case zipper.left(z) {
    Error(_) -> z
    Ok(zl) -> {
      case zipper.peek(z) {
        // Case: we already pushed this down. Don't touch it any more. 
        File(file_id, _) if last_id <= file_id ->
          compress_rec(zl, last_id, insert)
        Space(_) -> compress_rec(zl, last_id, insert)
        File(file_id, file_size) -> {
          let #(new_left_arm, rem) =
            zipper.left_arm(z)
            |> insert(file_id, file_size)

          let new_z = case rem {
            0 -> zipper.set(z, Space(file_size))
            _ -> {
              let new_z = zipper.set(z, File(file_id, rem))
              case file_size - rem {
                0 -> new_z
                _ -> zipper.insert_right(new_z, Space(file_size - rem))
              }
            }
          }
          new_z
          |> zipper.set_left_arm(new_left_arm)
          |> zipper.left
          |> result.map(compress_rec(_, file_id, insert))
          |> result.unwrap(new_z)
        }
      }
    }
  }
}

fn first_left_file(z: Zipper(Block)) -> Result(Int, Nil) {
  case zipper.peek(z) {
    Space(_) -> zipper.left(z) |> result.then(first_left_file)
    File(id, _) -> Ok(id)
  }
}

fn compress(l: List(Block), ins: Inserter) -> List(Block) {
  let z = zipper.from_list(l) |> zipper.back
  let assert Ok(last_id) = first_left_file(z)
  compress_rec(z, last_id + 1, ins) |> zipper.to_list
}

fn checksum(l: List(Block)) {
  use <- defer(fn(p) {
    let #(a, _) = p
    a
  })
  use #(sum, x), blk <- list.fold(l, #(0, 0))
  case blk {
    Space(size) -> #(sum, x + size)
    File(id, size) -> {
      let n =
        list.range(x, x + size - 1)
        |> list.map(fn(i) { id * i })
        |> int.sum

      #(sum + n, x + size)
    }
  }
}

pub fn day_main(day: util.AdventDay) {
  let assert Ok(data) = util.read_file(day.file) |> result.map(string.trim)
  let data = parse_summary(data)
  let d = compress(data, insert_no_frag)
  d
  |> checksum
  |> io.debug
  Nil
}
