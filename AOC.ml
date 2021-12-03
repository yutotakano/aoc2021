let aoc day part1 part2 =
  let result = function
    | [| _; "1" |] ->
        let file_contents () =
          Core.In_channel.read_lines (day ^ "part1.input.txt")
        in
        part1 (file_contents ())
    | [| _; "2" |] ->
        let file_contents () =
          Core.In_channel.read_lines (day ^ "part2.input.txt")
        in
        part2 (file_contents ())
    | _ ->
        Printf.printf "%s\n" "Usage: ./<executable> <1|2>";
        exit 1
  in

  Printf.printf "%s\n" (result (Core.Sys.get_argv ()))
