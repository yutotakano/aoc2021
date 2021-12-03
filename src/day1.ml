open AOC

let rec increases = function
| [] -> 0
| (x :: []) -> 0
| (x :: y :: xs) -> (if y > x then 1 else 0) + increases (y :: xs)

let rec increasesWindow = function
| [] -> 0
| (x :: []) -> 0
| (x :: y :: []) -> 0
| (x :: y :: z :: []) -> 0
| (x :: y :: z :: a :: xs) -> (if a > x then 1 else 0) + increasesWindow (y :: z :: a :: xs)

let part1 input = string_of_int (increases (List.map int_of_string input))
let part2 input = string_of_int (increasesWindow (List.map int_of_string input))

let () = aoc "day1" part1 part2
