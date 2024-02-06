
defmodule Day11 do
  def main() do
    monkeys_sample = %{
      0 => %{
        :op => fn x -> x * 19 end,
        :items => [79, 98],
        :next => {23, 2, 3},
        :numComp => 0
      },
      1 => %{
        :op => fn x -> x + 6 end,
        :items => [54, 65, 75, 74],
        :next => {19, 2, 0},
        :numComp => 0
      },
      2 => %{
        :op => fn x -> x * x end,
        :items => [79, 60, 97],
        :next => {13, 1, 3},
        :numComp => 0
      },
      3 => %{
        :op => fn x -> x + 3 end,
        :items => [74],
        :next => {17, 0, 1},
        :numComp => 0
      }
    }

    monkeys_test = %{
      0 => %{
        :op => fn x -> x * 5 end,
        :items => [92, 73, 86, 83, 65, 51, 55, 93],
        :next => {11, 3, 4},
        :numComp => 0
      },
      1 => %{
        :op => fn x -> x * x end,
        :items => [99, 67, 62, 61, 59, 98],
        :next => {2, 6, 7},
        :numComp => 0
      },
      2 => %{
        :op => fn x -> x * 7 end,
        :items => [81, 89, 56, 61, 99],
        :next => {5, 1, 5},
        :numComp => 0
      },
      3 => %{
        :op => fn x -> x + 1 end,
        :items => [97, 74, 68],
        :next => {17, 2, 5},
        :numComp => 0
      },
      4 => %{
        :op => fn x -> x + 3 end,
        :items => [78, 73],
        :next => {19, 2, 3},
        :numComp => 0
      },
      5 => %{
        :op => fn x -> x + 5 end,
        :items => [50],
        :next => {7, 1, 6},
        :numComp => 0
      },
      6 => %{
        :op => fn x -> x + 8 end,
        :items => [95, 88, 53, 75],
        :next => {3, 0, 7},
        :numComp => 0
      },
      7 => %{
        :op => fn x -> x + 2 end,
        :items => [50, 77, 98, 85, 94, 56, 89],
        :next => {13, 4, 0},
        :numComp => 0
      }
    }

    next = fn {{rnd, monkey}, monkeys} ->
      num_monkeys = Enum.count(monkeys)
      m = get_in(monkeys, [monkey])
      new_comp = m.numComp + Enum.count(m.items)
      monkeys = Enum.reduce(m.items, monkeys, fn (item, monkeys) ->
        new_worry = m.op.(item)
        {cnd, m1, m2} = m.next
        new_monkey = if rem(new_worry, cnd) == 0 do m1 else m2 end
        update_in(monkeys, [new_monkey, :items], fn ls -> ls ++ [new_worry] end)
      end)
      monkeys = put_in(monkeys, [monkey, :items], [])
        |> put_in([monkey, :numComp], new_comp)
      nxt = if monkey == num_monkeys - 1
        do {rnd + 1, 0}
        else {rnd, monkey + 1}
      end
      {nxt, monkeys}
    end

    {_, monkeys} = Stream.iterate({{0, 0}, monkeys_test}, next)
      |> Enum.take(10000 * 8 + 1)
      |> Enum.at(-1)
      |> IO.inspect()

    Map.values(monkeys)
      |> Enum.map(fn %{:numComp => n} -> n end)
      |> Enum.sort(:desc)
      |> Enum.take(2)
      |> Enum.product()
      |> IO.puts()

    # IO.inspect(final_monkeys)
  end
end

Day11.main()
