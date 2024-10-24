
# 2015

Language: Haskell (second time)

## Running

```
usage: cabal run x2015.cabal -- --day DAY [--file FNAME]

Options:
    DAY - The day # to run 
    FNAME - The file name under data/Day{DAY}/ 
            where the puzzle input is located.
            Defaults to "input.txt"
```


## Layout
The `app` folder contains code for each day. This year, I created a rudimentary
CLI for selecting a day and input dataset.

## Adding a day
A "Day" is any function `String -> IO ()` which accepts the puzzle input 
and runs the puzzle solution. Days are responsible for printing their output.

Once you define your day code in this format, you can register your day with
the CLI by adding it to the `days` list in `app/Days.hs`. Simply add a pair
to the list that is `(your day #, the day function)`, and the CLI defined
in `app/Main.hs` will allow users to run your day by providing the given day #
in the input flags.


