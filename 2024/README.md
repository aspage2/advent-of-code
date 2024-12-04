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

`use` expressions work for any function with a **callback** as its last argument:
* `result.then(res: Result(a, b), fn(a) -> Result(c, b)) -> Result(c, b)`
* `list.map(l: List(a), fn(a) -> b) -> List(b)`

Typical use of `list.map` has users defining an anonymous function to perform
the mapping:

```gleam
pub fn add_one(l: List(Int)) -> List(Int) {
    list.map(l, fn(x) { x + 1 })
}
```

Because `list.map` has a callback as its last parameter, this expression
can be translated into a `use` expression.

```gleam
pub fn add_one(l: List(Int)) -> List(Int) {
    use x <- list.map(l)
    x + 1
}
```
