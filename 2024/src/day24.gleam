
import gleam/order
import gleam/int 
import gleam/io
import gleam/bool
import gleam/result
import gleam/string
import util
import gleam/list
import gleam/dict.{type Dict}

pub type Op { AND OR XOR }

pub type Rule {
  Rule(left: String, op: fn(Bool, Bool) -> Bool, right: String, res: String)
}

fn normalizekey(k: #(String, String, Op)) {
  case string.compare(k.0, k.1) {
  order.Gt -> #(k.1, k.0, k.2)
  _ -> k
  }

}

pub type Wires = Dict(String, Bool)

pub fn parse(txt: String) -> #(Wires, List(Rule)) {
  let assert Ok(#(wires, rules)) = txt
    |> string.trim
    |> string.split_once("\n\n")

  let wires = wires
    |> string.split("\n")
    |> list.fold(dict.new(), fn(d, l) {
      let assert Ok(#(name, r)) = string.split_once(l, ": ")
      dict.insert(d, name, r == "1")
    })

  let rules = rules
    |> string.split("\n")
    |> list.map(fn(l) {
      let assert [l, op, r, "->", res] = string.split(l, " ")
      let op = case op {
        "AND" -> bool.and
        "OR" -> bool.or
        "XOR" -> bool.exclusive_or
        _ -> panic
      }
      Rule(l, op, r, res)
    })

  #(wires, rules)
}

pub fn simplify(wires: Wires, rules: List(Rule)) -> #(Wires, List(Rule)) {
  use #(ws, unresolved), rule <- list.fold(rules, #(wires, []))

  let res = {
    use l <- result.try(dict.get(ws, rule.left))
    use r <- result.map(dict.get(ws, rule.right))
    #(l, r)
  }
  case res {
  Ok(#(l, r)) -> #(dict.insert(ws, rule.res, rule.op(l, r)), unresolved)
  Error(_) -> #(ws, [rule, ..unresolved])
  }
}

pub fn fully_simplify(wires: Wires, rules: List(Rule)) -> Wires {
  use <- bool.guard(rules == [], wires)
  let #(a, b) = simplify(wires, rules)
  fully_simplify(a, b)
}

fn to_int(bs: List(Bool)) -> Int {
  use acc, b, i <- list.index_fold(bs, 0)
  bool.to_int(b)
    |> int.bitwise_shift_left(i)
    |> int.bitwise_or(acc)
}

pub fn parse2(txt: String) {
  let assert Ok(#(_, rules)) = txt |> string.trim |> string.split_once("\n\n")

  rules
    |> string.split("\n")
    |> list.fold(dict.new(), fn(d, l) {
      let assert [l, op, r, "->", res] = string.split(l, " ")
      let op = case op {
      "AND" -> AND
      "OR" -> OR
      "XOR" -> XOR
      _ -> panic
      }
      dict.insert(d, normalizekey(#(l, r, op)), res)
    })
}

// In part 2, we learn that the circuit is intended to
// be an adder circuit, with several gates with their
// outputs swapped. Through spot-checking a few connections,
// I learned that the circuit is intending to implement a
// ripple-carry adder: 
// https://en.wikipedia.org/wiki/Adder_(electronics)#/media/File:4-bit_ripple_carry_adder.svg
// In a ripple-carry adder, each bit # of the inputs connect
// to the corresponding output bit using a **full adder circuit**,
// with carry signals being passed between adjacent full adders
// for processing any carry bits. In other words, we calculate
// zN with bits xN and yN and the carry from adder (N-1).
//
// To find where the swaps are, we can have a script walk through
// the connections, looking for the expected logic gate patterns.
// If the configuration is correct, this function should be able to
// figure out all of the wires involved in a particular full-adder.
pub fn analyze_full_adder(a, b, cin, ref) -> Result(String, String) {
  let try = fn(x, y, op, name, nxt) {
    ref
      |> dict.get(normalizekey(#(x, y, op)))
      |> result.replace_error(name)
      |> result.try(nxt)
  }
  io.println("-- "<>a<>" --")
  use half_sum <- try(a, b, XOR, "half_sum")
  io.println("half sum: "<>half_sum)
  use half_carry <- try(a, b, AND, "half_carry")
  io.println("Half carry: "<>half_carry)
  use full_sum <- try(half_sum, cin, XOR, "full_sum")
  io.println("full sum: "<>full_sum)
  use mid_carry <- try(half_sum, cin, AND, "mid_carry")
  io.println("mid_carry: "<>mid_carry)
  use cout <- try(mid_carry, half_carry, OR, "cout")
  io.println("cout: "<>cout)
  Ok(cout)
}

pub fn try_until(i: Int, cin: String, ref) {
  let res = analyze_full_adder(
    "x"<>string.pad_start(int.to_string(i), 2, "0"),
    "y"<>string.pad_start(int.to_string(i), 2, "0"),
    cin,
    ref,
  )
  case res {
  Ok(cout) -> try_until(i+1, cout, ref)
  Error(_) -> res
  }
}

pub fn day_main(day: util.AdventDay) -> Nil {
  let assert Ok(data) = util.read_file(day.file)
  let ref = parse2(data)

  try_until(1, "rfg", ref)
  |> io.debug

  Nil
}
