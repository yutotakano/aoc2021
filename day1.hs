import Control.Monad
import AOC

main = aoc "day1" part1 part2

part1 input = increases $ map read input

part2 input = increasesWindow $ map read input

increases :: [Int] -> Int
increases [] = 0
increases (x:[]) = 0
increases (x:y:xs) = (if y > x then 1 else 0) + increases (y:xs)

increasesWindow :: [Int] -> Int
increasesWindow [] = 0
increasesWindow (x:[]) = 0
increasesWindow (x:y:[]) = 0
increasesWindow (x:y:z:[]) = 0
increasesWindow (x:y:z:a:xs) = (if a > x then 1 else 0) + increasesWindow (y:z:a:xs)
--  (y + z + a) > (x + y + z) is eqiuv to a > x
