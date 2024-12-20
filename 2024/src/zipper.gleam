import gleam/list

pub type Zipper(a) {
  Zipper(l: List(a), x: a, r: List(a))
}

fn guard_empty(l: List(a), default: b, f: fn(a, List(a)) -> b) -> b {
  case l {
    [] -> default
    [h, ..rest] -> f(h, rest)
  }
}

pub fn from_list(a: List(a)) -> Zipper(a) {
  let assert [h, ..rest] = a
  Zipper([], h, rest)
}

pub fn to_list(z: Zipper(a)) -> List(a) {
  z.l
  |> list.reverse
  |> list.append([z.x, ..z.r])
}

pub fn right(z: Zipper(a)) -> Result(Zipper(a), Nil) {
  use h, rest <- guard_empty(z.r, Error(Nil))
  Ok(Zipper(l: [z.x, ..z.l], x: h, r: rest))
}

pub fn left(z: Zipper(a)) -> Result(Zipper(a), Nil) {
  use h, rest <- guard_empty(z.l, Error(Nil))
  Ok(Zipper(l: rest, x: h, r: [z.x, ..z.r]))
}

pub fn front(z: Zipper(a)) -> Zipper(a) {
  let all =
    z.l
    |> list.reverse
    |> list.append([z.x, ..z.r])
  let assert [h, ..rest] = all
  Zipper([], h, rest)
}

pub fn back(z: Zipper(a)) -> Zipper(a) {
  let all =
    z.r
    |> list.reverse
    |> list.append([z.x, ..z.l])
  let assert [h, ..rest] = all
  Zipper(rest, h, [])
}

pub fn left_arm(z: Zipper(a)) -> List(a) {
  z.l
}

pub fn set_left_arm(z: Zipper(a), vals: List(a)) -> Zipper(a) {
  Zipper(..z, l: vals)
}

pub fn peek(z: Zipper(a)) -> a {
  z.x
}

pub fn set(z: Zipper(a), v: a) -> Zipper(a) {
  Zipper(..z, x: v)
}

pub fn insert_left(z: Zipper(a), v: a) -> Zipper(a) {
  Zipper(..z, l: [v, ..z.l])
}

pub fn insert_right(z: Zipper(a), v: a) -> Zipper(a) {
  Zipper(..z, r: [v, ..z.r])
}
