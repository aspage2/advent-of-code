import gleam/list

/// Push-left, pop-right deque
pub opaque type Deque(a) {
  Deque(left: List(a), right: List(a))
}

pub fn wrap(val: a) -> Deque(a) {
  Deque([val], [])
}

pub fn from_list(vals: List(a)) -> Deque(a) {
  Deque(vals, [])
}

pub fn to_list(d: Deque(a)) -> List(a) {
  d.right |> list.append(list.reverse(d.left))
}

pub fn push(d: Deque(a), val: a) -> Deque(a) {
  Deque(..d, left: [val, ..d.left])
}

pub fn push_left(d: Deque(a), val: a) -> Deque(a) {
  push(d, val)
}

pub fn pop(d: Deque(a)) -> Result(#(a, Deque(a)), Nil) {
  case d.right {
    [] ->
      case list.reverse(d.left) {
        [] -> Error(Nil)
        [ret, ..rest] -> Ok(#(ret, Deque([], rest)))
      }
    [ret, ..rest] -> Ok(#(ret, Deque(..d, right: rest)))
  }
}

pub fn pop_right(d: Deque(a)) -> Result(#(a, Deque(a)), Nil) {
  pop(d)
}

pub fn push_right(d: Deque(a), val: a) -> Deque(a) {
  Deque(..d, right: [val, ..d.right])
}

pub fn pop_left(d: Deque(a)) -> Result(#(a, Deque(a)), Nil) {
  case d.left {
    [] ->
      case list.reverse(d.right) {
        [] -> Error(Nil)
        [ret, ..rest] -> Ok(#(ret, Deque(rest, [])))
      }
    [ret, ..rest] -> Ok(#(ret, Deque(..d, left: rest)))
  }
}

pub fn fold(d: Deque(a), z: b, f: fn(b, a) -> b) -> b {
  list.fold(d.right, z, f) |> list.fold(list.reverse(d.left), _, f)
}
