
defmodule Day9 do

  def sgn(x) do
    cond do
      x < 0 -> -1
      x == 0 -> 0
      true -> 1
    end
  end

  def parse_line(l) do
    [direction, count] = String.split(l, " ")
    direction = case direction do
      "R" -> :right
      "L" -> :left
      "U" -> :up
      "D" -> :down
    end
    {count, _} = Integer.parse(count)
    {direction, count}
  end

  def move([{x, y} | tail], direction) do
    new_head = case direction do
      :right -> {x + 1, y}
      :left -> {x - 1, y}
      :down -> {x, y - 1}
      :up -> {x, y + 1}
    end
    {new_tail, new_tail_pos} = fix_tail(new_head, tail)
    {[new_head | new_tail], new_tail_pos}
  end

  def fix_tail(h, []) do
    # Return the tail position in the base case
    # because we need this for the puzzle answer.
    # Removes the need to traverse the list again
    # to get the last element.
    {[], h}
  end

  def fix_tail({hx, hy}, [{tx, ty} | ts]) do
    {dx, dy} = {hx - tx, hy - ty}

    {mx, my} = cond do
      abs(dx) > 1 || abs(dy) > 1 -> {sgn(dx), sgn(dy)}
      true -> {0, 0}
    end

    new_pos = {tx + mx, ty + my}
    {new_tail, last_pos} = fix_tail(new_pos, ts)
    {[new_pos | new_tail], last_pos}
  end

  def main() do
    [fname | _] = System.argv()

    # Simulate the rope as a list of N coordinates.
    rope = List.duplicate({0, 0}, 10)

    {visited, _} = File.read!(fname)
      |> String.split("\n")
      |> Enum.map(fn l -> parse_line(l) end)
      # Expand each direction count into N instances of that direction
      # as the "move" routine only supports moving by one space each
      # turn.
      |> Enum.flat_map(fn {d, n} -> List.duplicate(d, n) end)
      # Apply the "move" function on the rope for each direction
      # from the file. take note of the tail position in a "visit" set
      # we keep in the accumulator as well.
      |> Enum.reduce(
        {MapSet.new([{0, 0}]), rope},
        fn (d, {visited, rope}) ->
          {new_rope, tl_pos} = move(rope, d)
          {MapSet.put(visited, tl_pos), new_rope}
        end
      )
    IO.inspect(MapSet.size(visited))
  end
end

Day9.main()
