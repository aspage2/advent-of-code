
defmodule Day10 do
  def main() do
    [fname | _] = System.argv()

    {cycles, final} = File.read!(fname)
      |> String.split("\n")
      |> Enum.flat_map_reduce(1, fn (l, x) ->
        cond do
          String.starts_with?(l, "noop") -> {[x], x}
          true ->
            [_, inc | _] = String.split(l, " ")
            {inc, _} = Integer.parse(inc)
            {[x, x], x + inc}
        end
      end)

      Enum.concat(cycles, [final])
        |> Enum.with_index(1)
        |> Enum.filter(fn {_, i} -> rem(i - 20, 40) == 0 end)
        |> Enum.map(fn {x, i} -> x * i end)
        |> Enum.sum()
        |> IO.puts()

      Enum.concat(cycles, [final])
        |> Enum.with_index(1)
        |> Enum.map(fn {x, i} ->
          pos = rem(i - 1, 40)
          x - 1 <= pos && pos <= x + 1
        end)
        |> Enum.map(fn b -> if b do "#" else "." end end)
        |> printIt()
  end
  def printIt([]) do
  end
  def printIt(xs) do
    {row, rest} = Enum.split(xs, 40)
    IO.puts(Enum.join(row))
    printIt(rest)
  end
end

Day10.main()
