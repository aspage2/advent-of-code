import gleam/list
import gleam/int
import gleam/io
import gleam/option
import gleam/string
import gleam/result
import gleam/dict

import util

pub fn acc_line(l: String) -> #(Int, Int) {
	let assert Ok(#(p1, p2)) = string.split_once(l, "   ")
	let assert Ok(p1_parsed) = int.parse(p1) 
		|> result.replace_error("err parse p1")
	let assert Ok(p2_parsed) = int.parse(p2) 
		|> result.replace_error("err parse p2")
	#(p1_parsed, p2_parsed)
}

pub fn create_lists(l: List(String)) -> #(List(Int), List(Int)) {
	l |> list.map(acc_line) |> list.unzip
}

fn part1(left: List(Int), right: List(Int)) -> Int {
	let l_sorted = list.sort(left, int.compare)
	let r_sorted = list.sort(right, int.compare)
	list.zip(l_sorted, r_sorted)
		|> list.map(fn(pair){int.absolute_value(pair.0 - pair.1)})
		|> int.sum
}

fn part2(left: List(Int), right: List(Int)) -> Int {
	let counts = list.fold(right, dict.new(), fn(dct, i) {
		dict.upsert(dct, i, fn(v) {
			option.unwrap(v, 0) + 1
		})
	})
	left
		|> list.map(fn(x) {
			x * {dict.get(counts, x) |> result.unwrap(0)}
		})
		|> int.sum
}

pub fn day_main() {
	let assert Ok(data) = util.read_file("day2.txt") 
		|> result.map(string.trim)

	let rows = string.split(data, "\n")
	let #(l1, l2) = rows
		|> create_lists

	io.debug(part1(l1, l2))
	io.debug(part2(l1, l2))
}
