//// Counter - Utility class for creating and working with counts of things.
////
//// A counter is just an alias for a dictionary mapping keys to Ints, but
//// this library provides a few helper methods around the structure.

import gleam/int
import gleam/result
import gleam/list
import gleam/dict.{type Dict}
import gleam/option

/// A Counter maps keys to ints, oftentimes associated with a count value.
pub type Counter(a) = Dict(a, Int)

/// Increment the key by n
pub fn inc_by(c: Counter(a), k: a, n: Int) -> Counter(a) {
	use opt <- dict.upsert(c, k)
	case opt {
		option.None -> n
		option.Some(x) -> x + n
	}
}

/// Increment the key by 1
pub fn inc(c: Counter(a), k: a) -> Counter(a) {
	c |> inc_by(k, 1)
}

/// a new, empty counter.
pub fn new() -> Counter(a) {
	dict.new()
}

/// A new counter from the given list.
///
///  equivalent to new() |> extend(l)
pub fn from_keys(l: List(a)) -> Counter(a) {
	new() |> extend(l)
}

pub fn from_list(l: List(#(a, Int))) -> Counter(a) {
	dict.from_list(l)
}

/// Add a list of keys to the counter
pub fn extend(c: Counter(a), xs: List(a)) -> Counter(a) {
	list.fold(xs, c, inc)
}

/// Get the count of a key, or 0 if it doesn't exist.
pub fn get_count(c: Counter(a), k: a) -> Int {
	dict.get(c, k) |> result.unwrap(0)
}

pub fn total(c: Counter(a)) -> Int {
	c |> dict.values |> int.sum
}


/// Apply the given function to each key in the counter. Create a new
/// counter out of the resulting list elements, incrementing
/// each observed element by the original count of the key that produced
/// it.
///
/// Example:
///
/// from_list([#(1, 2), #(2, 1)]) |> multiply(fn(i) {[i-1, i, i + 1]})
/// -> from_list([#(0, 2), #(1, 3), #(2, 3), #(3, 1)])
///
pub fn multiply(c: Counter(a), f: fn(a) -> List(b)) -> Counter(b) {
	use new_c, k, cnt <- dict.fold(c, new())
	use new_c, k1 <- list.fold(f(k), new_c)
	new_c |> inc_by(k1, cnt)
}

/// Apply the function to all keys, and aggregate any 
/// duplicates that get produced.
///
/// Example:
///
///	from_list([#(3, 2), #(4, 1), #(11, 3), #(8, 1)]) |> map(fn(x) {x % 8})
/// -> from_list([#(0, 1), #(3, 5), #(4, 1)])
pub fn map(c: Counter(a), f: fn(a) -> b) -> Counter(b) {
	use new_c, k, cnt <- dict.fold(c, new())
	new_c |> inc_by(f(k), cnt)
}

