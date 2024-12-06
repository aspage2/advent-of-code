
import gleam/order
import gleam/bool
import gleam/dict
import gleam/list
import gleam/string
import gleam/io
import gleam/option
import gleam/result
import gleam/bit_array
import gleam/int
import gleam/set
import gleam/queue

import util

type Graph = dict.Dict(Int, List(Int))
type Pair = #(Int, Int)

fn root(g: Graph) -> Int {
	let keys = dict.keys(g) 
		|> set.from_list
	let keys_that_are_child = dict.values(g)
		|> list.flatten
		|> set.from_list
	io.debug(keys)
	io.debug(keys_that_are_child)
	
	let diff = keys
		|> set.difference(keys_that_are_child)
		|> set.to_list

	let assert [head, ..] = diff
	head
}

fn take_pairs_rec(acc: List(Pair), lines: List(String)) -> #(List(Pair), List(String)) {
	let f = fn(cont: fn(String, List(String)) -> #(List(Pair), List(String))){
		case lines {
			[] | ["", ..] -> #(acc, lines) 
			[x, ..rest] -> cont(x, rest)
		}
	}
	use x, rest <- f()

	let assert <<fst:bytes-size(2), "|", snd:bytes-size(2)>> = <<x:utf8>>

	let assert Ok(fst) = fst 
		|> bit_array.to_string() 
		|> result.then(int.parse)

	let assert Ok(snd) = snd
		|> bit_array.to_string() 
		|> result.then(int.parse)

	take_pairs_rec([#(fst, snd), ..acc], rest)
}

fn parse(txt: String) -> #(List(Pair), List(List(Int))) {
	let lines = string.trim(txt) |> string.split("\n") 
	let #(pairs, rest) = take_pairs_rec([], lines)
	let assert ["", ..rest] = rest

	let snd = rest
		|> list.map(fn(l) {
			string.split(l, ",") |> list.map(fn(x) {
				let assert Ok(x) = int.parse(x)
				x
			})
		})
	#(pairs, snd) 
}

// I don't know why this solution ended up so simple.
// it feels like we need to topologically sort the nodes
// according to the edge rules provided, but for some reason
// the puzzle input contained enough edges for us to simply
// be able to reference each edge in an inclusion set to determine
// if two nodes are in order.
//
// I wonder if there is a proof of this. Whereby for any G
fn is_in_order(l: List(Int), g: set.Set(#(Int, Int))) -> Bool {
	list.window_by_2(l) |> list.all(set.contains(g, _))
}

pub fn day_main(day: util.AdventDay) {
	let assert Ok(data) = util.read_file(day.file)
	let #(a, b) = parse(data)
	let ref = set.from_list(a)

	b	|> list.filter(is_in_order(_, ref))
		|> list.map(fn(l) {
			let len = list.length(l)
			let assert [head, ..] = list.drop(l, len / 2)
			head
		})
		|> int.sum
		|> io.debug

	b	|> list.filter(fn(x) {!is_in_order(x, ref)})
		|> list.map(fn(l) {
			use a, b <- list.sort(l)

			case set.contains(ref, #(a, b)) {
				True  -> order.Lt
				False -> order.Gt
			}
		})
		|> list.map(fn(l) {
			let len = list.length(l)
			let assert [head, ..] = list.drop(l, len / 2)
			head
		})
		|> int.sum
		|> io.debug
		
	Nil
}
