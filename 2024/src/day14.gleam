import gleam/pair
import gleam/set
import gleam/dict
import gleam/int
import gleam/io
import gleam/list
import gleam/option
import gleam/order.{Eq, Gt, Lt}
import gleam/result
import gleam/string
import gleam/string_tree

import parse.{type Parser}
import util

pub type Vec {
  Vec(x: Int, y: Int)
}

pub type Quadrant {
  I
  II
  III
  IV
  No
}

pub fn get_quadrant(pos: Vec, bounds: Vec) -> Quadrant {
  let x = int.compare(pos.x, bounds.x / 2)
  let y = int.compare(pos.y, bounds.y / 2)

  case x, y {
    Eq, _ | _, Eq -> No
    Lt, Lt -> I
    Gt, Lt -> II
    Lt, Gt -> III
    Gt, Gt -> IV
  }
}

pub fn parse_line() -> Parser(#(Vec, Vec)) {
  use _ <- parse.do(parse.drop(2))
  use p_x <- parse.do(parse.split_once(","))
  use p_y <- parse.do(parse.split_once(" "))
  use _ <- parse.do(parse.drop(2))
  use v_x <- parse.do(parse.split_once(","))
  use v_y <- parse.do(parse.get())

  let assert Ok(p_x) = int.parse(p_x)
  let assert Ok(p_y) = int.parse(p_y)
  let assert Ok(v_x) = int.parse(v_x)
  let assert Ok(v_y) = int.parse(v_y)

  #(Vec(p_x, p_y), Vec(v_x, v_y))
  |> parse.pure
}

fn translate1(p: Int, v: Int, bound: Int, time: Int) -> Int {
  let new_p = p + time * v
  case new_p < 0 {
    True -> {
      let n = int.absolute_value(new_p) / bound
      { new_p + { n + 1 } * bound } % bound
    }
    False -> new_p % bound
  }
}

pub fn translate(pos: Vec, vel: Vec, bounds: Vec, time: Int) -> Vec {
  Vec(
    translate1(pos.x, vel.x, bounds.x, time),
    translate1(pos.y, vel.y, bounds.y, time),
  )
}

pub fn counter(l: List(a)) -> dict.Dict(a, Int) {
  use cnt, x <- list.fold(l, dict.new())
  use opt <- dict.upsert(cnt, x)
  case opt {
    option.None -> 1
    option.Some(x) -> x + 1
  }
}

pub fn print(vs: List(Vec), bounds: Vec) -> String {
  let cnts = counter(vs)
  {
    use sb, y <- list.fold(list.range(0, bounds.y - 1), string_tree.new())
    let sb = {
      use sb, x <- list.fold(list.range(0, bounds.x - 1), sb)
      let grapheme = case dict.get(cnts, Vec(x, y)) {
        Error(Nil) -> "."
        Ok(x) -> int.to_string(x)
      }
      string_tree.append(sb, grapheme)
    }
    string_tree.append(sb, "\n")
  }
  |> string_tree.to_string
}

pub fn defer(f: fn(a) -> b, g: fn() -> a) -> b {
  f(g())
}

@external(erlang, "timer", "sleep")
pub fn sleep(ms: Int) -> util.Atom

pub fn robot_string(robots: List(Vec), bounds: Vec) -> String {
  let rs = set.from_list(robots)
  use <- defer(string_tree.to_string)

  use sb, r <- list.fold(list.range(0, bounds.y-1), string_tree.new())
  use <- defer(string_tree.append(_, "\n"))
  use sb, c <- list.fold(list.range(0, bounds.x-1), sb)
  string_tree.append(sb, case set.contains(rs, Vec(c, r)) {
    True -> "O"
    False -> " "
  })
}

pub fn day_main(day: util.AdventDay) -> Nil {
  let assert Ok(txt) =
    util.read_file(day.file)
    |> result.map(string.trim)

  let parser = parse_line()

  let robots =
    txt
    |> string.split("\n")
    |> list.map(parse.eval(parser, _))

  let b = Vec(101, 103)

  io.println(robot_string(robots |> list.map(pair.first), b))

  list.each(list.range(1000, 10000), fn(i) {
    let rs = robots |> list.map(fn(p) {
      translate(p.0, p.1, b, i)
    })
    io.println("\u{1b}[2J\u{1b}[H")
    io.println(int.to_string(i))
    io.println(robot_string(rs, b))
    sleep(500)
  })

  Nil
}
