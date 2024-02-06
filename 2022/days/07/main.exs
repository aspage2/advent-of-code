
defmodule Day7 do

  # --- Zipper pattern ---
  # The zipper pattern is a functional way to traverse a tree data structure
  # without needing to travel from the root node every time you move.
  # Basically, every time you go **down** a tree branch (chdir), you
  # leave a "breadcrumb", which in this case is the directory you are leaving
  # as well as its other child directories. To go **up** to a parent, you
  # just pick up the last breadcrumb (LIFO style) and re-build your file
  # tree from that.

  # This way, navigating the tree is a chain of function calls, much like you
  # would see in a real terminal

  # newRoot() |> addDir("myDir") |> addFile("myfile", 100) |> chDir("myDir") |> addFile("whatfile", 200)

  def newRoot() do
    {%{:dirs => %{}, :files => %{}}, []}
  end

  def chDir({cwd, bs}, dirname) do
    {dir, rest} = pop_in(cwd, [:dirs, dirname])
    {dir, [{rest, dirname} | bs]}
  end

  def chUp({cwd, []}) do
    {cwd, []}
  end
  def chUp({cwd, [{other, dn} | bs]}) do
      cwd = put_in(other, [:dirs, dn], cwd)
      {cwd, bs}
  end

  def addDir({cwd, bs}, dirname) do
    cwd = put_in(cwd, [:dirs, dirname], %{:files => %{}, :dirs => %{}})
    {cwd, bs}
  end

  def addFile({cwd, bs}, fname, fsize) do
    cwd = put_in(cwd, [:files, fname], fsize)
    {cwd, bs}
  end

  def top({cwd, []}) do
    cwd
  end

  def top(pair) do
    top(chUp(pair))
  end

  def parseLines(cwd, []) do
    cwd
  end

  def parseLines(cwd, [l | ls]) do
    args = String.split(l, " ")
    case Enum.at(args, 1) do
      "ls" ->
        {dirents, rest} = Enum.split_while(ls, fn l -> !String.starts_with?(l, "$") end)
        Enum.reduce(dirents, cwd, fn (line, wd) ->
          [arg1, arg2 | _] = String.split(line, " ")
          case arg1 do
            "dir" -> addDir(wd, arg2)
            _ -> addFile(wd, arg2, Integer.parse(arg1) |> elem(0))
          end
        end) |> parseLines(rest)
      "cd" ->
        case Enum.at(args, 2) do
          ".." -> chUp(cwd)
          dirname -> chDir(cwd, dirname)
        end
          |> parseLines(ls)
    end
  end

  def pr_directory(%{:dirs => ds, :files => fs}, ind) do
    pad = String.duplicate("| ", ind)
    if map_size(ds) > 0 do
      Map.to_list(ds) |> Enum.each(fn {dirname, dir} ->
        IO.puts("#{pad}#{dirname}/")
        pr_directory(dir, ind+1)
      end)
    end
    Map.to_list(fs) |> Enum.each(fn {fname, fsize} ->
      IO.puts("#{pad}#{fname} #{fsize}")
    end)
  end

  def dir_sizes(wd) do
    file_sizes = Map.values(wd.files) |> Enum.sum()
    dirs = Map.to_list(wd.dirs)
      |> Enum.map(fn {dirname, dir} ->
        {cwd_size, sizes} = dir_sizes(dir)
        [{dirname, cwd_size} | sizes]
      end)

    total_size = file_sizes + (Enum.map(dirs, &(elem(hd(&1), 1))) |> Enum.sum())
    {total_size, List.flatten(dirs)}
  end

  def main() do
    [fname | _] = System.argv()
    ls = File.read!(fname)
      |> String.split("\n")
      |> tl()
    wd = top(parseLines(newRoot(), ls))
    pr_directory(wd, 0)

    {root_size, dir_sizes} = dir_sizes(wd)

    # part 1 answer
    Enum.map(dir_sizes, &(elem(&1, 1)))
      |> Enum.filter(&(&1 <= 100000))
      |> Enum.sum()
      |> IO.puts()


    # part 2 answer
    current_free_space = 70000000 - root_size
    minimum_dir_size = 30000000 - current_free_space

    [{"/", root_size} | dir_sizes]
      |> Enum.map(&(elem(&1, 1)))
      |> Enum.filter(&(&1 >= minimum_dir_size))
      |> Enum.min()
      |> IO.puts()
  end
end

Day7.main()
