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
syntactic sugar around how functions are called.

Take for example this evaluator function which evaluates several
operations in a list:

```gleam
type Op {
    Neg(Int)
    Add(Int, Int)
    Mul(Int, Int)
}

fn evaluate_all(expressions: List(Op)) -> List(Int) {
    list.map(expressions, fn(exp) {
        case exp {
            Neg(x) -> -x
            Add(x, y) -> x + y
            Mul(x, y) -> x * y
        }
    })
}
```

Notice that we pass a **function** to `map` for computing
the new values of each list element. This is sometimes called a
"callback", since it's a function provided by outer code that
gets executed by inner code to do something.

A callback is just a normal function, it has parameters (like `exp`)
and a body (like the case statement). Since it is the last argument
to `map`, however, it is eligible for use within a `use` statement.

```gleam
fn evaluate_all(expressions: List(Op)) -> List(Int) {
//       ,----------------------------------.
//       V    parameters move here          |
    use exp <- list.map(expressions) // fn(exp) {

    case exp {
        Neg(x) -> -x
        Add(x, y) -> x + y // Function body shifts left by 1 indent
        Mul(x, y) -> x * y // <----------
    }

//  })
}
```

A `use` statement is sugar around callbacks. The `use` block has a **parameter**
list and **body**:

```gleam
// param list
//   V
use x, y <- my_func_with_callback("other", "args")
//  ~~~~
function() // Body
body()     // <<<<
```

Which, under the hood, turn into an anonymous function/closure
that is passed to the "used" function:

```gleam
// This is the same code!
my_func_with_callback("other", "args", fn(x, y) {
    function()
    body()
})
```
Once a `use` is invoked in your function, gleam will take
**all remaining function body** and treat it as a closure.
`use` essentially commandeers your function for the callback.

#### Neat! Why the hell would we do this?
`use` syntax enables pipelining, much like `|>`. The classic
pipeline is chaining error-prone operations with `result.then`:

```gleam
let user_fav_color = get_user_input()
    |> result.then(look_up_user_info(db, _))
    |> result.then(get_favorite_color)
```

With `result.then`, we avoid the hassle of nested `case` expressions,
using `then` to implicitly short-circuit when something returns an Error.

We can do something similar with `use`:

```gleam
fn user_fav_color() -> Result(String, Nil) {
    use user_id <- result.bind(get_user_input())
    use info <- look_up_user_info(user_id)
    get_favorite_color(info)
}
```

But why?

`use` is helpful when our "pipeline" wants to include intermediate
results during its processing.
