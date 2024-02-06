
winner = fn
  (a, b) when a == b -> :draw
  (:rock, :scissors) -> :left
  (:scissors, :paper) -> :left
  (:paper, :rock) -> :left
  (_, _) -> :right
end

parse_code = fn
  "A" -> :rock
  "X" -> :rock
  "B" -> :paper
  "Y" -> :paper
  "C" -> :scissors
  "Z" -> :scissors
end

parse_winner = fn
  "X" -> :left
  "Y" -> :draw
  "Z" -> :right
end

score = fn
  :rock -> 1
  :paper -> 2
  :scissors -> 3
end

[fname | _] = System.argv()


File.read!(fname)
  |> String.split("\n")
  |> Enum.map(fn line ->
      [fst , snd | _] = String.split(line, " ")
        |> Enum.map(parse_code)
      score.(snd) + case winner.(fst, snd) do
        :right -> 6
        :draw -> 3
        _ -> 0
      end
    end)
  |> Enum.sum()
  |> IO.puts()


exp_move = fn
  (a, :draw) -> a
  (:rock, :right) -> :paper
  (:rock, :left) -> :scissors
  (:paper, :right) -> :scissors
  (:paper, :left) -> :rock
  (:scissors, :right) -> :rock
  (:scissors, :left) -> :paper
end

File.read!(fname)
  |> String.split("\n")
  |> Enum.map(fn line ->
      [fst , snd | _] = String.split(line, " ")
      opponent_play = parse_code.(fst)
      outcome = parse_winner.(snd)

      score.(exp_move.(opponent_play, outcome)) + case outcome do
        :right -> 6
        :draw -> 3
        _ -> 0
      end
    end)
  |> Enum.sum()
  |> IO.puts()
