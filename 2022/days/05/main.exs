

[fname | _] = System.argv()

lines = File.read!(fname) |> String.split("\n")
{drawing, instructions} = Enum.split_while(lines, fn l -> l != "" end)

drawing = Enum.reverse(drawing)
num_columns = Kernel.div(String.length(hd(drawing)) + 1, 4)

mp = Enum.reduce(tl(drawing), %{}, fn (line, mp) ->
  Enum.reduce(0..(num_columns-1), mp, fn (i, mp2) ->
    chr = String.at(line, 4 * i + 1)
    cond do
      chr == nil || chr == " " -> mp2
      true -> Map.update(mp2, i, [chr], fn xs -> [chr | xs] end)
    end
  end)
end)

xs = tl(instructions)
  |> Enum.map(fn line ->
      vals = String.split(line, " ")
      {
        Enum.at(vals, 1) |> Integer.parse() |> elem(0),
        Enum.at(vals, 3) |> Integer.parse() |> elem(0),
        Enum.at(vals, 5) |> Integer.parse() |> elem(0)
      }
    end)
  |> Enum.reduce(mp, fn ({ct, from, to}, m) ->
    {top, rest} = Map.get(m, from-1) |> Enum.split(ct)
    m = Map.put(m, from-1, rest)
    Map.update!(m, to-1, fn xs -> top ++ xs end)
  end)

v = Enum.map(0..(num_columns-1), fn i ->
  hd(Map.get(xs, i))
end)

IO.inspect(Enum.join(v, ""))
