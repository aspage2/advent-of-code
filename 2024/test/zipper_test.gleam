import gleeunit
import gleeunit/should

import zipper.{type Zipper, Zipper}

/// A zipper should start at the left (all elements
/// except the head should be in the righthand list)
pub fn from_list_test() {
	zipper.from_list([1,2,3,4])
		|> should.equal(Zipper([], 1, [2,3,4]))
}

/// Translating a zipper back to a list should return
/// The entire contents of the zipper.
pub fn to_list_test() {
	Zipper([3, 2, 1], 4, [5, 6])
		|> zipper.to_list
		|> should.equal([1, 2, 3, 4, 5, 6])
}

/// Calling left should pop from the left list.
pub fn left_test() {
	Zipper([2, 1], 3, [4, 5])
		|> zipper.left
		|> should.be_ok
		|> should.equal(Zipper([1], 2, [3, 4, 5]))
}

/// Calling left at the front should error.
pub fn left_err_test() {
	Zipper([], 1, [2, 3])
		|> zipper.left
		|> should.be_error
}

/// Calling right should pop from the right list.
pub fn right_test() {
	Zipper([2, 1], 3, [4, 5])
		|> zipper.right
		|> should.be_ok
		|> should.equal(Zipper([3, 2, 1], 4, [5]))
}

/// Calling right when at the back should error
pub fn right_err_test() {
	Zipper([2, 1], 3, [])
		|> zipper.right
		|> should.be_error
}

/// Calling front on a zipper should return the
/// zipper pointed at the front.
pub fn front_test() {
	Zipper([3, 2, 1], 4, [])
		|> zipper.front
		|> should.equal(Zipper([], 1, [2, 3, 4]))
}

/// Calling front when at the front should return
/// the same zipper.
pub fn front2_test() {
	let z = Zipper([], 1, [2, 3])
	z |> zipper.front
	  |> should.equal(z)
}

/// Calling back should return the zipper pointed at
/// the back of the list
pub fn back_test() {
	Zipper([], 1, [2, 3, 4])
		|> zipper.back
		|> should.equal(Zipper([3, 2, 1], 4, []))
}

/// Calling back at the back should return the same zipper.
pub fn back2_test() {
	let z = Zipper([3, 2, 1], 4, [])
	z |> zipper.back
	  |> should.equal(z)
}

/// Peek should return the current pointer value
pub fn peek_test() {
	Zipper([2, 1], 3, [4])
		|> zipper.peek
		|> should.equal(3)
}

/// insert_left should add a new element to the lefthand list
pub fn insert_left_test() { 
	Zipper([1], 3, [4])
		|> zipper.insert_left(2)
		|> should.equal(Zipper([2, 1], 3, [4]))
}

/// insert_right should add a new element to the righthand list
pub fn insert_right_test() {
	Zipper([1, 2], 3, [])
		|> zipper.insert_right(4)
		|> should.equal(Zipper([1, 2], 3, [4]))
}

pub fn main() {
	gleeunit.main()
}


