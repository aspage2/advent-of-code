
defmodule Day8 do

  def parsegrid(input) do
    String.split(input, "\n")
      |> Enum.map(fn line ->
        String.graphemes(line)
          |> Enum.map(&(Integer.parse(&1) |> elem(0)))
          |> Enum.to_list()
          |> List.to_tuple()
        end)
      |> Enum.to_list()
      |> List.to_tuple()
  end

  def sight_lines({i, j}, nr, nc) do
    # The order of each coordinate in the sight line
    # is important for part 2.
    [
      (for n <- (i+1)..(nr-1), n < nr, do: {n, j}),
      (for n <- (i-1)..0, n >= 0, do: {n, j}),
      (for n <- (j-1)..0, n >= 0, do: {i, n}),
      (for n <- (j+1)..(nc-1), n < nc, do: {i, n})
    ]
  end

  def main() do
    [fname | _] = System.argv()
    grid = File.read!(fname)
      |> parsegrid()
    num_rows = Tuple.to_list(grid) |> Enum.count()
    num_cols = elem(grid, 0) |> Tuple.to_list() |> Enum.count()

    grid_get = fn {i, j} -> elem(grid, i) |> elem(j) end

    num_on_edge = 2 * num_rows + 2 * num_cols - 4

    center_trees = for i <- 1..(num_rows-2), j <- 1..(num_cols-2), do: {i, j}

    num_in_center = center_trees
      |> Enum.filter(fn coord ->
        coord_hgt = grid_get.(coord)
        sight_lines(coord, num_rows, num_cols)
          |> Enum.any?(fn line ->
            Enum.all?(line, &(grid_get.(&1) < coord_hgt))
          end)
        end)
      |> Enum.count()

    IO.puts(num_on_edge + num_in_center)

    clamp = fn
      (x, c) when x > c -> c
      (x, _) -> x
    end

    center_trees
      |> Enum.map(fn coord ->
        hgt = grid_get.(coord)
        l = sight_lines(coord, num_rows, num_cols)
          |> Enum.map(fn sl ->
            x = 1 + (Enum.take_while(sl, fn other -> hgt > grid_get.(other) end) |> Enum.count())
            clamp.(x, Enum.count(sl))
          end)
        Enum.product(l)
      end)
      |> Enum.max()
      |> IO.puts()
  end
end

Day8.main()
