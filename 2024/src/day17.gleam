import gleam/bool
import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import gleam/string_tree
import util

type Array(a)

@external(erlang, "array", "fix")
fn erl_array_fix(a: Array(a)) -> Array(a)

@external(erlang, "array", "from_list")
fn erl_arr_from_list(xs: List(a)) -> Array(a)

@external(erlang, "array", "get")
fn erl_array_get(i: Int, a: Array(a)) -> a

@external(erlang, "array", "size")
fn erl_array_size(a: Array(a)) -> Int

type Program =
  Array(Inst)

type VMState {
  VMState(pc: Int, reg_a: Int, reg_b: Int, reg_c: Int, output: List(Int))
}

type Inst {
  // adv x = $A / 2**C[x] -> $A
  Adv(Int)

  // bxl x = $B ^ x -> $B
  Bxl(Int)

  // bst x = C[x] & 0b111 -> $B
  Bst(Int)

  // jnz x = Jump to x if $A is not 0
  Jnz(pc: Int)

  // Bxc = $B ^ $C -> $B
  Bxc

  // out x = print C[x] % 0b111
  Out(Int)

  // bdv x = $A / 2**C[x] -> $B
  Bdv(Int)

  // cdv x = $A / 2**C[x] -> $C
  Cdv(Int)
}

fn combo_op(vs: VMState, num: Int) -> Int {
  case num {
    0 | 1 | 2 | 3 -> num
    4 -> vs.reg_a
    5 -> vs.reg_b
    6 -> vs.reg_c
    _ -> panic
  }
}

fn write_to_a(x: Int, vm: VMState) -> VMState {
  VMState(..vm, reg_a: x)
}

fn write_to_b(x: Int, vm: VMState) -> VMState {
  VMState(..vm, reg_b: x)
}

fn write_to_c(x: Int, vm: VMState) -> VMState {
  VMState(..vm, reg_c: x)
}

fn step(vs: VMState) -> VMState {
  VMState(..vs, pc: vs.pc + 1)
}

fn do(vs: VMState, inst: Inst) -> VMState {
  case inst {
    Adv(op) ->
      combo_op(vs, op)
      |> int.bitwise_shift_right(vs.reg_a, _)
      |> write_to_a(vs)
      |> step
    Bdv(op) ->
      combo_op(vs, op)
      |> int.bitwise_shift_right(vs.reg_a, _)
      |> write_to_b(vs)
      |> step
    Cdv(op) ->
      combo_op(vs, op)
      |> int.bitwise_shift_right(vs.reg_a, _)
      |> write_to_c(vs)
      |> step
    Bxl(l) ->
      vs.reg_b
      |> int.bitwise_exclusive_or(l)
      |> write_to_b(vs)
      |> step
    Bxc ->
      int.bitwise_exclusive_or(vs.reg_b, vs.reg_c)
      |> write_to_b(vs)
      |> step
    Bst(op) ->
      combo_op(vs, op)
      |> int.bitwise_and(0b111)
      |> write_to_b(vs)
      |> step
    Jnz(ad) ->
      case vs.reg_a == 0 {
        False -> VMState(..vs, pc: ad)
        True -> vs |> step
      }
    Out(op) ->
      combo_op(vs, op)
      |> int.bitwise_and(0b111)
      |> fn(x) { VMState(..vs, output: [x, ..vs.output]) }
      |> step
  }
}

fn unstep(vs: VMState) -> VMState {
  VMState(..vs, pc: vs.pc - 1)
}

fn run_program(vs: VMState, prgm: Program) -> VMState {
  use <- bool.guard(vs.pc >= erl_array_size(prgm), vs)

  let inst = erl_array_get(vs.pc, prgm)
  let next_vs = do(vs, inst)
  run_program(next_vs, prgm)
}

fn new_state() -> VMState {
  VMState(pc: 0, reg_a: 0, reg_b: 0, reg_c: 0, output: [])
}

fn to_string(vs: VMState) -> String {
  string_tree.from_strings([
    "<pc=",
    int.to_string(vs.pc),
    ", a=",
    int.to_string(vs.reg_a),
    ", b=",
    int.to_string(vs.reg_b),
    ", c=",
    int.to_string(vs.reg_c),
    ", out='",
    vs.output |> list.reverse |> list.map(int.to_string) |> string.join(","),
    "'>",
  ])
  |> string_tree.to_string
}

fn get_inst(i: Int, op: Int) -> Inst {
  case i {
    0 -> Adv(op)
    1 -> Bxl(op)
    2 -> Bst(op)
    3 -> Jnz(op)
    4 -> Bxc
    5 -> Out(op)
    6 -> Bdv(op)
    7 -> Cdv(op)
    _ -> panic
  }
}

fn pairs(l: List(a)) -> Result(List(#(a, a)), Nil) {
  case l {
    [] -> Ok([])
    [a, b, ..rest] -> pairs(rest) |> result.map(fn(ps) { [#(a, b), ..ps] })
    _ -> Error(Nil)
  }
}

fn check_program(vs: VMState, prgm: Program, check: List(Int)) -> Bool {
  use <- bool.guard(vs.pc >= erl_array_size(prgm), list.is_empty(check))

  let inst = erl_array_get(vs.pc, prgm)
  let next_vs = do(vs, inst)
  case inst {
    Out(op) -> {
      let o = combo_op(vs, op) |> int.bitwise_and(0b111)
      case check {
        [h, ..rest] if h == o -> check_program(next_vs, prgm, rest)
        _ -> False
      }
    }
    _ -> check_program(next_vs, prgm, check)
  }
}

fn compound_to_st(op: Int) -> String {
  case op {
    0 | 1 | 2 | 3 -> int.to_string(op)
    4 -> "A"
    5 -> "B"
    6 -> "C"
    _ -> panic
  }
}

fn inst_to_string(inst: Inst) -> String {
  case inst {
    Out(x) -> "out " <> compound_to_st(x)
    Adv(x) -> "A = A >> " <> compound_to_st(x)
    Bdv(x) -> "B = A >> " <> compound_to_st(x)
    Cdv(x) -> "C = A >> " <> compound_to_st(x)
    Bxl(l) -> "B = B ^ " <> int.to_string(l)
    Bst(x) -> "B = " <> compound_to_st(x) <> " % 8"
    Jnz(loc) -> "jnz " <> int.to_string(loc)
    Bxc -> "B = B ^ C"
  }
}

fn find(prgm: Program, rev_p: List(Int), a: Int) -> Int {
  case rev_p {
    [] -> a
    [h, ..rest] -> {
      let opts =
        list.range(0, 7)
        |> list.map(fn(x) {
          int.bitwise_shift_left(a, 3)
          |> int.bitwise_or(x)
        })
      let f =
        list.filter(opts, fn(x) {
          let p = run_program(new_state() |> write_to_a(x, _), prgm)
          io.debug(to_string(p))
          p.reg_b |> int.bitwise_and(0b111) == h
        })

      io.debug(f)

      let assert [hd, ..] = f

      io.debug("Found " <> int.to_string(hd))
      find(prgm, rest, hd)
    }
  }
}

pub fn day_main(d: util.AdventDay) {
  let p = [2, 4, 1, 2, 7, 5, 4, 5, 0, 3, 1, 7, 5, 5, 3, 0]

  let assert Ok(prgm_list) =
    p
    |> pairs
    |> result.map(list.map(_, fn(p) { get_inst(p.0, p.1) }))

  prgm_list |> list.each(fn(a) { io.println(inst_to_string(a)) })

  let prgm =
    prgm_list
    |> list.take(6)
    |> erl_arr_from_list

  io.debug(prgm)

  find(prgm, p |> list.reverse, 0) |> io.debug
  Nil
}
