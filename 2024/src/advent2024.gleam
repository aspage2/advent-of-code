import gleam/io
import gleam/result
import util

import day1
import day2
import day3
import day4
import day5
import day6
import day7
import day8
import day9
import day10
import day11
import day12
import day13
import day14
import day15
import day16

import scratch

type AdventMain = fn(util.AdventDay) -> Nil 

const days: List(AdventMain) = [
	day1.day_main,
	day2.day_main,
	day3.day_main,
	day4.day_main,
	day5.day_main,
	day6.day_main,
	day7.day_main,
	day8.day_main,
	day9.day_main,
	day10.day_main,
	day11.day_main,
	day12.day_main,
	day13.day_main,
	day14.day_main,
	day15.day_main,
	day16.day_main,
]

fn get_day(i: Int, days: List(AdventMain)) -> Result(AdventMain, String) {
	case days {
		[] -> 
			Error("No such day. did you remember to add it to src/advent2024.gleam?")
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
