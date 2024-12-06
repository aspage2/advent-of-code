import gleam/io
import gleam/result
import util

import day1
import day2
import day3
import day4
import day5

import scratch

type AdventMain = fn(util.AdventDay) -> Nil 

const days: List(AdventMain) = [
	day1.day_main,
	day2.day_main,
	day3.day_main,
	day4.day_main,
	day5.day_main,
]

fn get_day(i: Int, days: List(AdventMain)) -> Result(AdventMain, String) {
	case days {
		[] -> Error("list index out of range")
		[x, .._] if i <= 0 -> Ok(x)
		[_, ..rest] -> get_day(i - 1, rest)
	}
}

fn main_func() -> Result(Nil, String) {
	case util.argv() {
		[] -> Ok(scratch.run())
		args -> {
			use day <- result.try(util.parse_args(args))
			use fnc <- result.try(get_day(day.day-1, days))
			Ok(fnc(day))
		}
	}
}

pub fn main() {
	case main_func() {
		Error(msg) -> io.print_error(msg)
		_ -> Nil
	}
}
