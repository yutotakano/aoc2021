open AOC

type instruction = Move of int * int

let result (a, b) = a * b

let parseInstructions input =
  let [instr; amount] = String.split_on_char ' ' input in
  match instr with
    | "up" -> Move (0, (-1) * int_of_string amount)
    | "down" -> Move (0, int_of_string amount)
    | "forward" -> Move (int_of_string amount, 0)
    | _ -> Move (0, 0)

let rec exec = function
| [] -> (0, 0)
| (Move (a, b) :: xs) -> let (x, y) = exec xs in (x + a, y + b)

let part1 input = string_of_int (result (exec (List.map parseInstructions input)))

let part2 input = assert false

let () = aoc "day2" part1 part2
