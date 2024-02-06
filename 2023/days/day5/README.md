# Day 5

## Part I

Though tricky to parse, Part 1 was manageable. Each section is called a
"Map" in my code, with each map containing a vector of all the "rules"
that applied to that map. When doing a lookup, the map just compares the
seed's value to the rules to see which rule the seed applies to.

Some things I noticed in the input:
* Values were incredibly large; I was getting integer overflow
  errors using u32, so I bumped to u64.
* Map rules were disjoint - the input ranges never overlapped.
  this means we should only expect at most one rule to apply
  to a particular value.
* The order of the maps (seed -> soil, soil -> water, etc)
  in the input were the same order as I would need to perform lookups
  for each seed. This meant that I could load the maps into a vector
  and traverse one after the other without having to sort the maps first.

## Part II

This is one of those AoC problems where they somehow manage to drastically
increase the number of inputs to the problem despite having the same input
text. In Part I, there were around 10-20 seeds. In part 2, interpreting
pairs as **ranges** of seed meant that there were upwards of 3.5 billion inputs.

For giggles, I tried my Part I solution on the input space and cancelled it after
15 minutes.

Rather than hyper-optimize my code and brute-force the solution, I opted to
implement a solution that relied on **interval math**. The solution relies on
the fact that you can process a **range of seeds** in a map without needing to
**perform a lookup on each seed in the range**. This works because **lookups apply
the same left/right shift to every seed in the lookup's input range**. A particular
range [x, y) with all of its values shifted left by d units is equivalent to a
range [x + d, y + d). This means that we only need to operate on the endpoints
of the range to represent a shift from every value within it.


