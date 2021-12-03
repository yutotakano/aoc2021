open Base
open AOC

let result (a, b) = a * b

let (|-) x f = f x; x

type instruction1 = Move of int * int

let parse_instructions1 input =
  let [instr; amount] = String.split_on_chars ~on:[' '] input in
  match instr with
  | "up"      -> Move (0, (-1) * Int.of_string amount)
  | "down"    -> Move (0, Int.of_string amount)
  | "forward" -> Move (Int.of_string amount, 0)
  | _         -> Move (0, 0)

let rec exec1 = function
| []                  -> (0, 0)
| (Move (a, b) :: xs) -> let (x, y) = exec1 xs in (x + a, y + b)

let part1 input =
  input
  |> List.map ~f:parse_instructions1
  |> exec1
  |> result
  |> Int.to_string

type instruction2
  = Forward of int
  | ShiftAim of int

let parse_instructions2 input =
  let [instr; amount] = String.split_on_chars ~on:[' '] input in
  match instr with
  | "up"      -> ShiftAim ((-1) * Int.of_string amount)
  | "down"    -> ShiftAim (Int.of_string amount)
  | "forward" -> Forward (Int.of_string amount)

let rec exec2 (x, y) aim = function
| []                 -> (x, y)
| (Forward n :: xs)  -> exec2 (x + n, y + (aim * n)) aim xs
| (ShiftAim n :: xs) -> exec2 (x, y) (aim + n) xs

let part2 input =
  input
  |> List.map ~f:parse_instructions2
  |> exec2 (0, 0) 0
  |> result
  |> Int.to_string

let () = aoc "day2" part1 part2
