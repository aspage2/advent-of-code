
import gleam/string
import gleam/bit_array
import gleam/result

pub type Atom

@external(erlang, "file", "read_file")
pub fn erl_read_file(fname: String) -> Result(BitArray, Atom)

@external(erlang, "erlang", "atom_to_list")
pub fn erl_atom_to_list(a: Atom) -> List(UtfCodepoint) 

pub fn read_file(fname: String) -> Result(String, String) {
	use bin <- result.try(erl_read_file(fname)
		|> result.map_error(fn(a) { 
			erl_atom_to_list(a) |> string.from_utf_codepoints
		})
	)

	bit_array.to_string(bin)
		|> result.map_error(fn(_) { "not a valid bit sequence" })
}


