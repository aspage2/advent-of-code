# File Parsing
[fname | _] = System.argv()

calorie_counts = File.read!(fname)
  |> String.split("\n")
  |> Stream.unfold(fn
      [] -> nil
      ls ->
        {elf_pack, rest} = Enum.split_while(ls, &(&1 != ""))
        total_calories = elf_pack
          |> Enum.map(&(Integer.parse(&1) |> elem(0)))
          |> Enum.sum()
        rest = Enum.drop_while(rest, &(&1 == ""))
        {total_calories, rest}
    end)

# Part 1
IO.puts(Enum.max(calorie_counts))

# Part 2
calorie_counts
  |> Enum.sort(:desc)
  |> Enum.take(3)
  |> Enum.sum()
  |> IO.puts()
