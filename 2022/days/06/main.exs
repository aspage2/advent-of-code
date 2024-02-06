
[input | _] = System.argv()
n = 14
i = Enum.find(0..(String.length(input) - n), nil, fn i ->
  num_uniq = String.slice(input, i, n)
    |> String.graphemes()
    |> Enum.uniq()
    |> Enum.count()
  num_uniq == n
end)


IO.puts(i + n)
