# advent2024

For 2024, I will try my luck at Gleam: https://gleam.run/

Gleam is a strict, statically-typed functional language that compiles to
[BEAM](https://en.wikipedia.org/wiki/BEAM_(Erlang_virtual_machine)) or Javascript.

Gleam is functional-first, with arbitrary i/o allowed in statements. 

### Elixir & Erlang
It bears similarities to its sister BEAM languages. It has support for 
Erlang's binary syntax (i.e. representing bit strings with `<<1, 2, 3>>` 
notation. It also has support for Elixir's pipeline `|>` operator.

Interop with erlang is easy as well; with the `@external` directive,
gleam programs can call the erlang standard library functions, or any
user code written in Erlang. Much like Scala, Gleam's standard library
has limited system-level calls, requiring users to pull in many i/o
functions from Erlang.

### Static Typing
Gleam is statically typed, unlike its BEAM sisters. Static typing does well
at eliminating pesky typing bugs that happen in languages like Python - 
code fails to compile unless all expressions can be resolved in full to a
single type.

Gleam is very strict, even requiring different numeric operators for different
numeric types. For example, addition has two operators: `+` for ints, and `+.`
for floating-point numbers.

### `use` syntactic sugar
`use` is a bit like Haskell's `do` notation, in that it's really just
syntactic sugar for some other language operation.

For Haskell, `do` notation collects several monadic operations and calls 

