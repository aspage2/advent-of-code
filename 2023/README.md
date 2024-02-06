# Advent of Code 2023

I use AoC to learn new programming languages. I challenge myself to work through AoC
in a new language every year.

This year, I chose Rust as my language.


## Project structure

### `days` folder
Contains each day of the calendar as a rust crate (is it called a crate or module?).

To create a new day, I simply run:

```
cargo new --bin days/dayN
```

### `data` folder
Is where I store input data for each day. It's loosely structured as a flat folder containing each
day's input. My naming convention is:

* `dayX.txt` is the real puzzle input
* `dayX-sample.txt` is the sample input from the prompt.


### Running a day

Most days, you select which day folder you want to run and supply the day's input (either the sample or the real input):
```
cargo run --bin dayX data/dayX.txt
```

## Rust

I've spent a good-ly amount of time looking for a language that emphasizes functional programming in
an accessible way.

Python iterators and generators are cool, it has a huge standard library, and it has a massive community. But I do everything in Python, which is boooring.

My first AoC I tried Haskell; awesome for learning **pure** fp, but one of the steepest learning
curves I've ever experienced. Not gonna lie, I'm just pretending to know what the hell monads are.

My second one I tried Elixir. I recall it being interesting, but it never stuck with me.

Rust feels like it has all of the things I liked about the previous languages:
* Strong typing
* Good standard library
* Iterators & Closures
* Algebraic data types and pattern matching
* Huge community.

It has all of the good qualities of a functional language without ever preventing you from stepping
back into an imperative mindset. It means that you can choose the paradigm that best fits the
circumstance you're in.

I think that coming from Haskell & Go, the incredibly strict typing in Rust isn't a blocker for me.
It also helps that I have `rust_analyzer` hooked up to my editor.

I haven't had too much issue with the borrow-checker either? I've read through the memory management
section of the rust book a few times now. I also realize that I'm using a very small subset of
the language, and the code I'm writing is dense but not complex.

