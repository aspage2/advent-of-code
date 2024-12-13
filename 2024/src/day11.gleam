import gleam/dict.{type Dict}
import gleam/io
import gleam/string
import gleam/bit_array
import gleam/list
import gleam/int
import gleam/float
import gleam/bool
import gleam/option
import util
import gleam/result

const main_input = "2701 64945 0 9959979 93 781524 620 1"
const example_input = "125 17"


@external(erlang, "math", "log10")
fn erl_log10(x: Int) -> Float

fn num_digits(x: Int) -> Int {
	let x = erl_log10(x)
		|> float.floor
		|> float.round
	x + 1
}

fn next(l: Dict(Int, Int)) -> Dict(Int, Int) {
	use acc, num, count <- dict.fold(l, dict.new())
	let res = {
		use <- bool.guard(num == 0, [1])
		let num_digits = num_digits(num)
		case num_digits % 2 == 0 {
		True -> {
			let nover2 = num_digits/2
			let s = int.to_string(num)
			let assert <<fst:bytes-size(nover2), snd:bytes>> = <<s:utf8>>
			let assert Ok(x) = result.all([
				bit_array.to_string(fst) |> result.then(int.parse),
				bit_array.to_string(snd) |> result.then(int.parse),
			])
			x
		}
		False -> [2024 * num]
		}
	}

	use d, n <- list.fold(res, acc)
	use opt <- dict.upsert(d, n)
	case opt {
	option.None -> count
	option.Some(x) -> count + x
	}
}

pub fn day_main(_: util.AdventDay) {
	let assert Ok(data) = main_input
		|> string.split(" ")
		|> list.map(int.parse)
		|> result.all
	
	let ugh = data |> list.map(fn(x) {#(x, 1)}) |> dict.from_list
	let res = list.fold(list.range(1, 25), ugh, fn(acc, _) { next(acc) })
	io.debug(dict.values(res) |> int.sum)
	let res = list.fold(list.range(1, 50), ugh, fn(acc, _) { next(acc) })
	io.debug(dict.values(res) |> int.sum)
	Nil
}

