
parse_line = fn line ->
  [fst, snd | _] = String.split(line, ",")
  [fst_lower, fst_upper | _] = String.split(fst, "-")
    |> Enum.map(&Integer.parse/1)
    |> Enum.map(fn x -> elem(x, 0) end)
  [snd_lower, snd_upper | _] = String.split(snd, "-")
    |> Enum.map(&Integer.parse/1)
    |> Enum.map(fn x -> elem(x, 0) end)
  {{fst_lower, fst_upper}, {snd_lower, snd_upper}}
end

fully_contains = fn {{w, x}, {y, z}} ->
  (w <= y && z <= x) || (y <= w && x <= z)
end

pairs_overlap = fn {{w, x}, {y, z}} ->
  (w >= y && w <= z)
    || (x >= y && x <= z)
    || (y >= w && y <= x)
    || (z >= w && z <= x)
end


[fname | _] = System.argv()

all_pairs = File.read!(fname)
  |> String.split("\n")
  |> Enum.map(parse_line)
  |> Enum.filter(pairs_overlap)
  |> Enum.count()



IO.puts(all_pairs)
