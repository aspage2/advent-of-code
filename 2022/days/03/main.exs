
charset = fn str ->
  String.graphemes(str) |> MapSet.new()
end

priority = fn c ->
  <<h::utf8, _::binary>> = c
  cond do
    h >= ?a && h <= ?z -> h - ?a + 1
    h >= ?A && h <= ?Z -> h - ?A + 27
  end
end

get_common = fn line ->
  n = String.length(line)
  {comp1, comp2} = String.split_at(line, Kernel.div(n, 2))
  diff = MapSet.intersection(charset.(comp1), charset.(comp2))

  hd(MapSet.to_list(diff))
end

[fname | _] = System.argv()

total = File.read!(fname)
  |> String.split("\n")
  |> Enum.map(get_common)
  |> Enum.map(priority)
  |> Enum.sum()

IO.puts(total)

common_char = fn {a, b, c} ->
  aSet = MapSet.new(String.graphemes(a))
  bSet = MapSet.new(String.graphemes(b))
  cSet = MapSet.new(String.graphemes(c))
  diff = MapSet.intersection(aSet, bSet)
    |> MapSet.intersection(cSet)
  hd(MapSet.to_list(diff))
end

File.read!(fname)
  |> String.split("\n")
  |> Stream.unfold(fn
    [] -> nil
    xs ->
      [a, b, c | rest] = xs
      {{a, b, c}, rest}
  end)
  |> Enum.map(common_char)
  |> Enum.map(priority)
  |> Enum.sum()
  |> IO.puts()
